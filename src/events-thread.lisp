(in-package :ls-base)

;;;;;; Thread based communication

(defun read-loop (connection)
  (let ((input-stream (connection-socket-io connection))
        (control-thread (mconn.control-thread connection)))
    (with-slynk-error-handler (connection)
      (loop (send control-thread (decode-message input-stream))))))

(defun dispatch-loop (connection)
  (let ((*emacs-connection* connection))
    (with-panic-handler (connection)
      (loop (dispatch-event connection (receive))))))

(defvar *auto-flush-interval* 0.2)

(defun auto-flush-loop (stream)
  (loop
   (when (not (and (open-stream-p stream)
                   (output-stream-p stream)))
     (return nil))
   (force-output stream)
   (sleep *auto-flush-interval*)))

(defgeneric thread-for-evaluation (connection id)
  (:documentation "Find or create a thread to evaluate the next request.")
  (:method ((connection multithreaded-connection) (id (eql t)))
    (spawn-worker-thread connection))
  (:method ((connection multithreaded-connection) (id (eql :find-existing)))
    (car (mconn.active-threads connection)))
  (:method (connection (id integer))
    (declare (ignorable connection))
    (find-thread id))
  (:method ((connection singlethreaded-connection) id)
    (declare (ignorable connection connection id))
    (current-thread)))

(defun interrupt-worker-thread (connection id)
  (let ((thread (thread-for-evaluation connection
                                       (cond ((eq id t) :find-existing)
                                             (t id)))))
    (log-event "interrupt-worker-thread: ~a ~a~%" id thread)
    (if thread
        (etypecase connection
          (multithreaded-connection
           (interrupt-thread thread
                             (lambda ()
                               ;; safely interrupt THREAD
                               (invoke-or-queue-interrupt #'simple-break))))
          (singlethreaded-connection
           (simple-break)))
        (encode-message (list :debug-condition (current-thread-id)
                              (format nil "Thread with id ~a not found"
                                      id))
                        (current-socket-io)))))

(defun spawn-worker-thread (connection)
  (spawn (lambda ()
           (with-bindings *default-worker-thread-bindings*
             (with-top-level-restart (connection nil)
               (apply #'eval-for-emacs
                      (cdr (wait-for-event `(:emacs-rex . _)))))))
         :name "slynk-worker"))

(defun add-active-thread (connection thread)
  (etypecase connection
    (multithreaded-connection
     (push thread (mconn.active-threads connection)))
    (singlethreaded-connection)))

(defun remove-active-thread (connection thread)
  (etypecase connection
    (multithreaded-connection
     (setf (mconn.active-threads connection)
           (delete thread (mconn.active-threads connection) :count 1)))
    (singlethreaded-connection)))

(defun dispatch-event (connection event)
  "Handle an event triggered either by Emacs or within Lisp."
  (log-event "dispatch-event: ~s~%" event)
  (destructure-case event
    ((:emacs-rex form package thread-id id &rest extra-rex-options)
     (let ((thread (thread-for-evaluation connection thread-id)))
       (cond (thread
              (add-active-thread connection thread)
              (send-event thread `(:emacs-rex ,form ,package ,id ,@extra-rex-options)))
             (t
              (encode-message
               (list :invalid-rpc id
                     (format nil "Thread not found: ~s" thread-id))
               (current-socket-io))))))
    ((:return thread &rest args)
     (remove-active-thread connection thread)
     (encode-message `(:return ,@args) (current-socket-io)))
    ((:emacs-interrupt thread-id)
     (interrupt-worker-thread connection thread-id))
    (((:write-string
       :debug :debug-condition :debug-activate :debug-return :channel-send
       :presentation-start :presentation-end
       :new-package :new-features :ed :indentation-update
       :eval :eval-no-wait :background-message :inspect :ping
       :y-or-n-p :read-from-minibuffer :read-string :read-aborted :test-delay)
      &rest _)
     (declare (ignore _))
     (encode-message event (current-socket-io)))
    (((:emacs-pong :emacs-return :emacs-return-string) thread-id &rest args)
     (send-event (find-thread thread-id) (cons (car event) args)))
    ((:emacs-channel-send channel-id msg)
     (let* ((ch (find-channel channel-id))
            (thread (and ch (find-channel-thread ch))))
       (cond ((and ch thread)
              (send-event thread `(:emacs-channel-send ,ch ,msg)))
             (ch
              (encode-message 
               (list :invalid-channel channel-id
                     "No suitable threads for channel")
               (current-socket-io)))
             (t
              (encode-message 
               (list :invalid-channel channel-id "Channel not found")
               (current-socket-io))))))
    ((:reader-error packet condition)
     (encode-message `(:reader-error ,packet
                                     ,(safe-condition-message condition))
                     (current-socket-io)))))


(defun send-event (thread event)
  (log-event "send-event: ~s ~s~%" thread event)
  (let ((c *emacs-connection*))
    (etypecase c
      (multithreaded-connection
       (send thread event))
      (singlethreaded-connection
       (setf (sconn.event-queue c) (nconc (sconn.event-queue c) (list event)))
       (setf (sconn.events-enqueued c) (mod (1+ (sconn.events-enqueued c))
                                            most-positive-fixnum))))))

(defun send-to-emacs (event)
  "Send EVENT to Emacs."
  ;;(log-event "send-to-emacs: ~a" event)
  (without-sly-interrupts
    (let ((c *emacs-connection*))
      (etypecase c
        (multithreaded-connection
         (send (mconn.control-thread c) event))
        (singlethreaded-connection
         (dispatch-event c event)))
      (maybe-slow-down))))

(defun make-thread-bindings-aware-lambda (fn)
  (let ((connection *emacs-connection*)
        (send-counter *send-counter*))
    (lambda (&rest args)
      (let ((*emacs-connection* connection)
            (*send-counter* send-counter))
        (apply fn args)))))
