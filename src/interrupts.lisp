(in-package :langserver-base)

;;;; Interrupt handling

;; Usually we'd like to enter the debugger when an interrupt happens.
;; But for some operations, in particular send&receive, it's crucial
;; that those are not interrupted when the mailbox is in an
;; inconsistent/locked state. Obviously, if send&receive don't work we
;; can't communicate and the debugger will not work.  To solve that
;; problem, we try to handle interrupts only at certain safe-points.
;;
;; Whenever an interrupt happens we call the function
;; INVOKE-OR-QUEUE-INTERRUPT.  Usually this simply invokes the
;; debugger, but if interrupts are disabled the interrupt is put in a
;; queue for later processing.  At safe-points, we call
;; CHECK-SLY-INTERRUPTS which looks at the queue and invokes the
;; debugger if needed.
;;
;; The queue for interrupts is stored in a thread local variable.
;; WITH-CONNECTION sets it up.  WITH-CLIENT-INTERRUPTS allows
;; interrupts, i.e. the debugger is entered immediately.  When we call
;; "user code" or non-problematic code we allow interrupts.  When
;; inside WITHOUT-CLIENT-INTERRUPTS, interrupts are queued.  When we
;; switch from "user code" to more delicate operations we need to
;; disable interrupts.  In particular, interrupts should be disabled
;; for SEND and RECEIVE-IF.

;; If true execute interrupts, otherwise queue them.
;; Note: `with-connection' binds *pending-sly-interrupts*.
(defvar *client-interrupts-enabled*)

(defmacro with-interrupts-enabled% (flag body)
  `(progn
     ,@(if flag '((check-sly-interrupts)))
     (multiple-value-prog1
         (let ((*client-interrupts-enabled* ,flag))
           ,@body)
       ,@(if flag '((check-sly-interrupts))))))

(defmacro with-client-interrupts (&body body)
  `(with-interrupts-enabled% t ,body))

(defmacro without-client-interrupts (&body body)
  `(with-interrupts-enabled% nil ,body))

(defun invoke-or-queue-interrupt (function)
  (log-event "invoke-or-queue-interrupt: ~a~%" function)
  (cond ((not (boundp '*client-interrupts-enabled*))
         (without-client-interrupts
           (funcall function)))
        (*client-interrupts-enabled*
         (log-event "interrupts-enabled~%")
         (funcall function))
        (t
         (setq *pending-sly-interrupts*
               (nconc *pending-sly-interrupts*
                      (list function)))
         (cond ((cdr *pending-sly-interrupts*)
                (log-event "too many queued interrupts~%")
                (with-simple-restart (continue "Continue from interrupt")
                  (handler-bind ((serious-condition #'invoke-sly-debugger))
                    (check-sly-interrupts))))
               (t
                (log-event "queue-interrupt: ~a~%" function)
                (when *interrupt-queued-handler*
                  (funcall *interrupt-queued-handler*)))))))

;; Thread local variable used for flow-control.
;; It's bound by `with-connection'.
(defvar *send-counter*)

(defmacro with-connection ((connection) &body body)
  "Execute BODY in the context of CONNECTION."
  `(let ((connection ,connection)
         (function (lambda () . ,body)))
     (if (eq *emacs-connection* connection)
         (funcall function)
         (let ((*emacs-connection* connection)
               (*pending-sly-interrupts* '())
               (*send-counter* 0))
           (without-client-interrupts
             (with-langserver-error-handler (connection)
               (with-default-listener (connection)
                 (call-with-debugger-hook #'langserver-debugger-hook
                                          function))))))))

(defun call-with-retry-restart (msg thunk)
  (loop (with-simple-restart (retry "~a" msg)
          (return (funcall thunk)))))

(defmacro with-retry-restart ((&key (msg "Retry.")) &body body)
  (check-type msg string)
  `(call-with-retry-restart ,msg (lambda () ,@body)))

(defmacro with-struct* ((conc-name get obj) &body body)
  (let ((var (gensym)))
    `(let ((,var ,obj))
       (macrolet ((,get (slot)
                    (let ((getter (intern (concatenate 'string
                                                       ',(string conc-name)
                                                       (string slot))
                                          (symbol-package ',conc-name))))
                      `(,getter ,',var))))
         ,@body))))
