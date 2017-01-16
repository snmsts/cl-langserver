(in-package :langserver-base)

;;;;; Misc

(defun use-threads-p ()
  (eq (connection-communication-style *client-connection*) :spawn))

(defun current-thread-id ()
  (thread-id (current-thread)))

(declaim (inline ensure-list))
(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))
