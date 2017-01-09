(in-package :ls-base)

;;;;;; Simple sequential IO

(defun simple-serve-requests (connection)
  (unwind-protect
       (with-connection (connection)
         (call-with-user-break-handler
          (lambda ()
            (invoke-or-queue-interrupt
             (lambda () (dispatch-interrupt-event connection))))
          (lambda ()
            (with-simple-restart (close-connection "Close SLY connection.")
              (let* ((stdin (real-input-stream *standard-input*))
                     (*standard-input* (make-repl-input-stream connection
                                                               stdin)))
                (tagbody toplevel
                   (with-top-level-restart (connection (go toplevel))
                     (simple-repl))))))))
    (close-connection connection nil (safe-backtrace))))

;; this is signalled when our custom stream thinks the end-of-file is reached.
;; (not when the end-of-file on the socket is reached)
(define-condition end-of-repl-input (end-of-file) ())

(defun simple-repl ()
  (loop
   (format t "~a> " (package-string-for-prompt *package*))
   (force-output)
   (let ((form (handler-case (read)
                 (end-of-repl-input () (return)))))
     (let ((- form)
           (values (multiple-value-list (eval form))))
       (setq *** **  ** *  * (car values)
             /// //  // /  / values
             +++ ++  ++ +  + form)
       (cond ((null values) (format t "; No values~&"))
             (t (mapc (lambda (v) (format t "~s~&" v)) values)))))))

(defun make-repl-input-stream (connection stdin)
  (make-input-stream
   (lambda () (repl-input-stream-read connection stdin))))

(defun repl-input-stream-read (connection stdin)
  (loop
   (let* ((socket (connection-socket-io connection))
          (inputs (list socket stdin))
          (ready (wait-for-input inputs)))
     (cond ((eq ready :interrupt)
            (check-sly-interrupts))
           ((member socket ready)
            ;; A Sly request from Emacs is pending; make sure to
            ;; redirect IO to the REPL buffer.
            (with-simple-restart (process-input "Continue reading input.")
              (let ((*sly-db-quit-restart* (find-restart 'process-input)))
                (with-default-listener (connection)
                  (handle-requests connection t)))))
           ((member stdin ready)
            ;; User typed something into the  *inferior-lisp* buffer,
            ;; so do not redirect.
            (return (read-non-blocking stdin)))
           (t (assert (null ready)))))))

(defun read-non-blocking (stream)
  (with-output-to-string (str)
    (handler-case
        (loop (let ((c (read-char-no-hang stream)))
                (unless c (return))
                (write-char c str)))
      (end-of-file () (error 'end-of-repl-input :stream stream)))))
