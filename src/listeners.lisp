(in-package :langserver-base)

;;; Listeners
(defclass listener ()
  ((out :initarg :out :type stream :reader listener-out)
   (in  :initarg :in :type stream :reader listener-in)
   (env)))

(defmacro listeners () `(connection-listeners *client-connection*))

(defmethod initialize-instance :after ((l listener) &key initial-env) 
  (with-slots (out in env) l
    (let ((io (make-two-way-stream in out)))
      (setf env
            (append
             initial-env
             `((cl:*standard-output* . ,out)
               (cl:*standard-input*  . ,in)
               (cl:*trace-output*    . ,out)
               (cl:*error-output*    . ,out)
               (cl:*debug-io*        . ,io)
               (cl:*query-io*        . ,io)
               (cl:*terminal-io*     . ,io)))))
    (assert out nil "Must have an OUT stream")
    (assert in nil "Must have an IN stream")
    (assert env nil "Must have an ENV"))
  (setf (listeners) (nconc (listeners)
                           (list l))))

(defun call-with-listener (listener fn &optional saving)
  (with-slots (env) listener
    (with-bindings env
      (unwind-protect (funcall fn)
        (when saving
          (loop for binding in env
                do (setf (cdr binding) (symbol-value (car binding)))))))))

(defmacro with-listener-bindings (listener &body body)
  "Execute BODY inside LISTENER's environment"
  `(call-with-listener ,listener (lambda () ,@body)))

(defmacro saving-listener-bindings (listener &body body)
  "Execute BODY inside LISTENER's environment, update it afterwards."
  `(call-with-listener ,listener (lambda () ,@body) 'saving))

(defmacro with-default-listener ((connection) &body body)
  "Execute BODY with in CONNECTION's default listener."
  (let ((listener-sym (gensym))
        (body-fn-sym (gensym)))
    `(let ((,listener-sym (default-listener ,connection))
           (,body-fn-sym #'(lambda () ,@body)))
       (if ,listener-sym
           (with-listener-bindings ,listener-sym
             (funcall ,body-fn-sym))
           (funcall ,body-fn-sym)))))

(defun default-listener (connection)
  (first (connection-listeners connection)))

(defun flush-listener-streams (listener)
  (with-slots (in out) listener
    (force-output out)
    (clear-input in)))

(defmethod close-listener (l)
  ;; TODO: investigate why SBCL complains when we close IN and OUT
  ;; here.
  (setf (listeners) (delete l (listeners))))
