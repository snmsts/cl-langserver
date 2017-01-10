(in-package :ls-base)
;;;;;; Flow control

;; After sending N (usually 100) messages we slow down and ping Emacs
;; to make sure that everything we have sent so far was received.

(defconstant send-counter-limit 100)

(defun maybe-slow-down ()
  (let ((counter (incf *send-counter*)))
    (when (< send-counter-limit counter)
      (setf *send-counter* 0)
      (ping-pong))))

(defun ping-pong ()
  (let* ((tag (make-tag))
         (pattern `(:emacs-pong ,tag)))
    (send-to-emacs `(:ping ,(current-thread-id) ,tag))
    (wait-for-event pattern)))


(defun wait-for-event (pattern &optional timeout)
  "Scan the event queue for PATTERN and return the event.
If TIMEOUT is NIL wait until a matching event is enqued.
If TIMEOUT is T only scan the queue without waiting.
The second return value is t if the timeout expired before a matching
event was found."
  (log-event "wait-for-event: ~s ~s~%" pattern timeout)
  (without-sly-interrupts
    (let ((c *emacs-connection*))
      (etypecase c
        (multithreaded-connection
         (receive-if (lambda (e) (event-match-p e pattern)) timeout))
        (singlethreaded-connection
         (wait-for-event/event-loop c pattern timeout))))))

(defun wait-for-event/event-loop (connection pattern timeout)
  (assert (or (not timeout) (eq timeout t)))
  (loop
   (check-sly-interrupts)
   (let ((event (poll-for-event connection pattern)))
     (when event (return (car event))))
   (let ((events-enqueued (sconn.events-enqueued connection))
         (ready (wait-for-input (list (current-socket-io)) timeout)))
     (cond ((and timeout (not ready))
            (return (values nil t)))
           ((or (/= events-enqueued (sconn.events-enqueued connection))
                (eq ready :interrupt))
            ;; rescan event queue, interrupts may enqueue new events
            )
           (t
            (assert (equal ready (list (current-socket-io))))
            (dispatch-event connection
                            (decode-message (current-socket-io))))))))

(defun poll-for-event (connection pattern)
  (let* ((c connection)
         (tail (member-if (lambda (e) (event-match-p e pattern))
                          (sconn.event-queue c))))
    (when tail
      (setf (sconn.event-queue c)
            (nconc (ldiff (sconn.event-queue c) tail) (cdr tail)))
      tail)))

;;; FIXME: Make this use SLYNK-MATCH.
(defun event-match-p (event pattern)
  (cond ((or (keywordp pattern) (numberp pattern) (stringp pattern)
	     (member pattern '(nil t)))
	 (equal event pattern))
	((symbolp pattern) t)
	((consp pattern)
         (case (car pattern)
           ((or) (some (lambda (p) (event-match-p event p)) (cdr pattern)))
           (t (and (consp event)
                   (and (event-match-p (car event) (car pattern))
                        (event-match-p (cdr event) (cdr pattern)))))))
        (t (error "Invalid pattern: ~S" pattern))))



(defun spawn-threads-for-connection (connection)
  (setf (mconn.control-thread connection)
        (spawn (lambda () (control-thread connection))
               :name "control-thread"))
  connection)

(defun control-thread (connection)
  (with-struct* (mconn. @ connection)
    (setf (@ control-thread) (current-thread))
    (setf (@ reader-thread) (spawn (lambda () (read-loop connection))
                                   :name "reader-thread"))
    (setf (@ indentation-cache-thread)
          (spawn (lambda () (indentation-cache-loop connection))
                 :name "slynk-indentation-cache-thread"))
    (dispatch-loop connection)))

(defun cleanup-connection-threads (connection)
  (let* ((c connection)
         (threads (list (mconn.reader-thread c)
                        (mconn.control-thread c)
                        (mconn.auto-flush-thread c)
                        (mconn.indentation-cache-thread c))))
    (dolist (thread threads)
      (when (and thread
                 (thread-alive-p thread)
                 (not (equal (current-thread) thread)))
        (kill-thread thread)))))
