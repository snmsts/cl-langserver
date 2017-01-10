(in-package :ls-base)

;;;;; Event Processing

(defvar *sly-db-quit-restart* nil
  "The restart that will be invoked when the user calls sly-db-quit.")

;; Establish a top-level restart and execute BODY.
;; Execute K if the restart is invoked.
(defmacro with-top-level-restart ((connection k) &body body)
  `(with-connection (,connection)
     (restart-case
         (let ((*sly-db-quit-restart* (find-restart 'abort)))
           ,@body)
       (abort (&optional v)
         :report "Return to SLY's top level."
         (declare (ignore v))
         (force-user-output)
         ,k))))

(defun handle-requests (connection &optional timeout)
  "Read and process :emacs-rex requests.
The processing is done in the extent of the toplevel restart."
  (with-connection (connection)
    (cond (*sly-db-quit-restart*
           (process-requests timeout))
          (t
           (tagbody
            start
              (with-top-level-restart (connection (go start))
                (process-requests timeout)))))))

(defun process-requests (timeout)
  "Read and process requests from Emacs.
TIMEOUT has the same meaning as in WAIT-FOR-EVENT."
  (catch 'stop-processing
    (loop
      (multiple-value-bind (event timed-out-p)
          (wait-for-event `(or (:emacs-rex . _)
                               (:emacs-channel-send . _))
                          timeout)
        (when timed-out-p (return))
        (destructure-case event
          ((:emacs-rex &rest args) (apply #'eval-for-emacs args))
          ((:emacs-channel-send channel (selector &rest args))
           (channel-send channel selector args)))))))

(defun spawn-channel-thread (connection channel)
  "Spawn a listener thread for CONNECTION and CHANNEL.

The new thread will block waiting for a :SERVE-CHANNEL message, then
process all requests in series until the :TEARDOWN message, at which
point the thread terminates and CHANNEL is closed."
  (ls-backend:spawn
   (lambda ()
     (with-connection (connection)
       (destructure-case
        (ls-backend:receive)
        ((:serve-channel c)
         (assert (eq c channel))
         (loop
           (with-top-level-restart (connection
                                    (drop-unprocessed-events channel))
             
             (when (eq (process-requests nil)
                       'listener-teardown)
               (return))))))
       (close-channel channel)))
   :name (with-slots (id name) channel
           (format nil "sly-channel-~a-~a" id name))))


(defun current-socket-io ()
  (connection-socket-io *emacs-connection*))

(defun close-connection (connection condition backtrace)
  (send-to-sentinel `(:close-connection ,connection ,condition ,backtrace)))

(defun close-connection% (c condition backtrace)
  (let ((*debugger-hook* nil))
    (log-event "close-connection: ~a ...~%" condition)
    (format *log-output* "~&;; slynk:close-connection: ~A~%"
            (escape-non-ascii (safe-condition-message condition)))
    (let ((*emacs-connection* c))
      (format *log-output* "~&;; closing ~a channels~%" (length (connection-channels c)))
      (mapc #'close-channel (connection-channels c))
      (format *log-output* "~&;; closing ~a listeners~%" (length (connection-listeners c)))
      (mapc #'close-listener (connection-listeners c)))
    (stop-serving-requests c)
    (close (connection-socket-io c))
    (setf *connections* (remove c *connections*))
    (run-hook *connection-closed-hook* c)
    (when (and condition (not (typep condition 'end-of-file)))
      (finish-output *log-output*)
      (format *log-output* "~&;; Event history start:~%")
      (dump-event-history *log-output*)
      (format *log-output* "~
;; Event history end.~%~
;; Backtrace:~%~{~A~%~}~
;; Connection to Emacs lost. [~%~
;;  condition: ~A~%~
;;  type: ~S~%~
;;  style: ~S]~%"
              (loop for (i f) in backtrace
                    collect
                    (ignore-errors
                     (format nil "~d: ~a" i (escape-non-ascii f))))
              (escape-non-ascii (safe-condition-message condition) )
              (type-of condition)
              (connection-communication-style c)))
    (finish-output *log-output*)
    (log-event "close-connection ~a ... done.~%" condition)))
