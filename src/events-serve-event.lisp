(in-package :ls-base)

;;;;;; SERVE-EVENT based IO

(defun install-fd-handler (connection)
  (add-fd-handler (connection-socket-io connection)
                  (lambda () (handle-requests connection t)))
  (setf (sconn.saved-sigint-handler connection)
        (install-sigint-handler
         (lambda ()
           (invoke-or-queue-interrupt
            (lambda () (dispatch-interrupt-event connection))))))
  (handle-requests connection t))

(defun dispatch-interrupt-event (connection)
  (with-connection (connection)
    (dispatch-event connection `(:emacs-interrupt ,(current-thread-id)))))

(defun deinstall-fd-handler (connection)
  (log-event "deinstall-fd-handler~%")
  (remove-fd-handlers (connection-socket-io connection))
  (install-sigint-handler (sconn.saved-sigint-handler connection)))
