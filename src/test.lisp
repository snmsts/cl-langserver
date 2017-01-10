(in-package :ls-base)
;;;; Testing

(defslyfun io-speed-test (&optional (n 1000) (m 1))
  (let* ((s *standard-output*)
         (*trace-output* (make-broadcast-stream s *log-output*)))
    (time (progn
            (dotimes (i n)
              (format s "~D abcdefghijklm~%" i)
              (when (zerop (mod n m))
                (finish-output s)))
            (finish-output s)
            (when *emacs-connection*
              (eval-in-emacs '(message "done.")))))
    (terpri *trace-output*)
    (finish-output *trace-output*)
    nil))

(defslyfun flow-control-test (n delay)
  (let ((stream (make-output-stream
                 (let ((conn *emacs-connection*))
                   (lambda (string)
                     (declare (ignore string))
                     (with-connection (conn)
                       (send-to-emacs `(:test-delay ,delay))))))))
    (dotimes (i n)
      (print i stream)
      (force-output stream)
      (background-message "flow-control-test: ~d" i))))
