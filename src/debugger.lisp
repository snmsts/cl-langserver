(in-package :ls-base)
;;;; Debugger
(defun invoke-sly-debugger (condition)
  "Sends a message to Emacs declaring that the debugger has been entered,
then waits to handle further requests from Emacs. Eventually returns
after Emacs causes a restart to be invoked."
  (without-sly-interrupts
    (cond (*emacs-connection*
           (debug-in-emacs condition))
          ((default-connection)
           (with-connection ((default-connection))
             (debug-in-emacs condition))))))

(define-condition invoke-default-debugger () ())

(defun slynk-debugger-hook (condition hook)
  "Debugger function for binding *DEBUGGER-HOOK*."
  (declare (ignore hook))
  (handler-case
      (call-with-debugger-hook #'slynk-debugger-hook
                               (lambda () (invoke-sly-debugger condition)))
    (invoke-default-debugger ()
      (invoke-default-debugger condition))))

(defun invoke-default-debugger (condition)
  (call-with-debugger-hook nil (lambda () (invoke-debugger condition))))

(defvar *global-debugger* t
  "Non-nil means the Slynk debugger hook will be installed globally.")

(add-hook *new-connection-hook* 'install-debugger)
(defun install-debugger (connection)
  (declare (ignore connection))
  (when *global-debugger*
    (install-debugger-globally #'slynk-debugger-hook)))

;;;;; Debugger loop
;;;
;;; These variables are dynamically bound during debugging.
;;;
(defvar *slynk-debugger-condition* nil
  "The condition being debugged.")

(defvar *sly-db-level* 0
  "The current level of recursive debugging.")

(defvar *sly-db-initial-frames* 20
  "The initial number of backtrace frames to send to Emacs.")

(defvar *sly-db-restarts* nil
  "The list of currenlty active restarts.")

(defvar *sly-db-stepping-p* nil
  "True during execution of a step command.")

(defun debug-in-emacs (condition)
  (let ((*slynk-debugger-condition* condition)
        (*sly-db-restarts* (compute-restarts condition))
        (*sly-db-quit-restart* (and *sly-db-quit-restart*
                                  (find-restart *sly-db-quit-restart*)))
        (*package* (or (and (boundp '*buffer-package*)
                            (symbol-value '*buffer-package*))
                       *package*))
        (*sly-db-level* (1+ *sly-db-level*))
        (*sly-db-stepping-p* nil))
    (force-user-output)
    (call-with-debugging-environment
     (lambda ()
       (sly-db-loop *sly-db-level*)))))

(defun sly-db-loop (level)
  (unwind-protect
       (loop
        (with-simple-restart (abort "Return to sly-db level ~D." level)
          (send-to-emacs
           (list* :debug (current-thread-id) level
                  (debugger-info-for-emacs 0 *sly-db-initial-frames*)))
          (send-to-emacs
           (list :debug-activate (current-thread-id) level nil))
          (loop
           (handler-case
               (destructure-case (wait-for-event
                                  `(or (:emacs-rex . _)
                                       (:emacs-channel-send . _)
                                       (:sly-db-return ,(1+ level))))
                 ((:emacs-rex &rest args) (apply #'eval-for-emacs args))
                 ((:emacs-channel-send channel (selector &rest args))
                  (channel-send channel selector args))
                 ((:sly-db-return _) (declare (ignore _)) (return nil)))
             (sly-db-condition (c)
               (handle-sly-db-condition c))))))
    (send-to-emacs `(:debug-return
                     ,(current-thread-id) ,level ,*sly-db-stepping-p*))
    (wait-for-event `(:sly-db-return ,(1+ level)) t) ; clean event-queue
    (when (> level 1)
      (send-event (current-thread) `(:sly-db-return ,level)))))

(defun handle-sly-db-condition (condition)
  "Handle an internal debugger condition.
Rather than recursively debug the debugger (a dangerous idea!), these
conditions are simply reported."
  (let ((real-condition (original-condition condition)))
    (send-to-emacs `(:debug-condition ,(current-thread-id)
                                      ,(princ-to-string real-condition)))))

(defun %%condition-message (condition)
  (let ((limit (ash 1 16)))
    (with-string-stream (stream :length limit)
      (handler-case
          (let ((*print-readably* nil)
                (*print-pretty* t)
                (*print-right-margin* 65)
                (*print-circle* t)
                (*print-length* (or *print-length* limit))
                (*print-level* (or *print-level* limit))
                (*print-lines* (or *print-lines* limit)))
            (print-condition condition stream))
        (serious-condition (c)
          (ignore-errors
            (with-standard-io-syntax
              (let ((*print-readably* nil))
                (format stream "~&Error (~a) printing the following condition: " (type-of c))
                (print-unreadable-object (condition stream :type t
                                                    :identity t))))))))))

(defun %condition-message (condition)
  (string-trim #(#\newline #\space #\tab)
               (%%condition-message condition)))

(defvar *sly-db-condition-printer* #'%condition-message
  "Function called to print a condition to an SLY-DB buffer.")

(defun safe-condition-message (condition)
  "Print condition to a string, handling any errors during printing."
  (funcall *sly-db-condition-printer* condition))

(defvar *debugger-extra-options* nil
  ;; JT@15/08/24: FIXME: Actually, with a nice and proper method-combination for
  ;; interfaces (as was once quite bravely attempted by Helmut, this variable
  ;; could go away and contribs could simply add methods to CONDITION-EXTRAS)
  ;; 
  "A property list of extra options describing a condition.
This works much like the CONDITION-EXTRAS interface, but can be
dynamically bound by contribs when invoking the debugger.")

(defun debugger-condition-for-emacs ()
  (list (safe-condition-message *slynk-debugger-condition*)
        (format nil "   [Condition of type ~S]"
                (type-of *slynk-debugger-condition*))
        (append (condition-extras *slynk-debugger-condition*)
                *debugger-extra-options*)))

(defun format-restarts-for-emacs ()
  "Return a list of restarts for *slynk-debugger-condition* in a
format suitable for Emacs."
  (let ((*print-right-margin* most-positive-fixnum))
    (loop for restart in *sly-db-restarts* collect
          (list (format nil "~:[~;*~]~a"
                        (eq restart *sly-db-quit-restart*)
                        (restart-name restart))
                (with-output-to-string (stream)
                  (without-printing-errors (:object restart
                                            :stream stream
                                            :msg "<<error printing restart>>")
                    (princ restart stream)))))))

;;;;; SLY-DB entry points

(defslyfun sly-db-break-with-default-debugger (dont-unwind)
  "Invoke the default debugger."
  (cond (dont-unwind
         (invoke-default-debugger *slynk-debugger-condition*))
        (t
         (signal 'invoke-default-debugger))))

(defslyfun backtrace (start end)
  "Return a list ((I FRAME PLIST) ...) of frames from START to END.

I is an integer, and can be used to reference the corresponding frame
from Emacs; FRAME is a string representation of an implementation's
frame."
  (loop for frame in (compute-backtrace start end)
        for i from start collect
        (list* i (frame-to-string frame)
               (ecase (frame-restartable-p frame)
                 ((nil) nil)
                 ((t) `((:restartable t)))))))

(defun frame-to-string (frame)
  (with-string-stream (stream :length (* (or *print-lines* 1)
                                         (or *print-right-margin* 100))
                              :bindings *backtrace-printer-bindings*)
    (handler-case (print-frame frame stream)
      (serious-condition ()
        (format stream "[error printing frame]")))))

(defslyfun debugger-info-for-emacs (start end)
  "Return debugger state, with stack frames from START to END.
The result is a list:
  (condition ({restart}*) ({stack-frame}*) (cont*))
where
  condition   ::= (description type [extra])
  restart     ::= (name description)
  stack-frame ::= (number description [plist])
  extra       ::= (:references and other random things)
  cont        ::= continutation
  plist       ::= (:restartable {nil | t | :unknown})

condition---a pair of strings: message, and type.  If show-source is
not nil it is a frame number for which the source should be displayed.

restart---a pair of strings: restart name, and description.

stack-frame---a number from zero (the top), and a printed
representation of the frame's call.

continutation---the id of a pending Emacs continuation.

Below is an example return value. In this case the condition was a
division by zero (multi-line description), and only one frame is being
fetched (start=0, end=1).

 ((\"Arithmetic error DIVISION-BY-ZERO signalled.
Operation was KERNEL::DIVISION, operands (1 0).\"
   \"[Condition of type DIVISION-BY-ZERO]\")
  ((\"ABORT\" \"Return to Sly toplevel.\")
   (\"ABORT\" \"Return to Top-Level.\"))
  ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\" (:restartable nil)))
  (4))"
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)
        *pending-continuations*))

(defun nth-restart (index)
  (nth index *sly-db-restarts*))

(defslyfun invoke-nth-restart (index)
  (let ((restart (nth-restart index)))
    (when restart
      (let* ((prompt nil)
             (*query-io*
               (make-two-way-stream
                (make-input-stream
                 (lambda ()
                   (format nil "~a~%"
                           (read-from-minibuffer-in-emacs
                            (format nil "~a" (or prompt
                                                 "[restart prompt] :"))))))
                (make-output-stream
                 #'(lambda (s)
                     (setq prompt
                           (concatenate 'string
                                        (or prompt "")
                                        s)))))))
        (invoke-restart-interactively restart)))))

(defslyfun sly-db-abort ()
  (invoke-restart (find 'abort *sly-db-restarts* :key #'restart-name)))

(defslyfun sly-db-continue ()
  (continue))

(defun coerce-to-condition (datum args)
  (etypecase datum
    (string (make-condition 'simple-error :format-control datum
                            :format-arguments args))
    (symbol (apply #'make-condition datum args))))

(defslyfun simple-break (&optional (datum "Interrupt from Emacs") &rest args)
  (with-simple-restart (continue "Continue from break.")
    (invoke-sly-debugger (coerce-to-condition datum args))))

;; FIXME: (last (compute-restarts)) looks dubious.
(defslyfun throw-to-toplevel ()
  "Invoke the ABORT-REQUEST restart abort an RPC from Emacs.
If we are not evaluating an RPC then ABORT instead."
  (let ((restart (or (and *sly-db-quit-restart*
                          (find-restart *sly-db-quit-restart*))
                     (car (last (compute-restarts))))))
    (cond (restart (invoke-restart restart))
          (t (format nil "Restart not active [~s]" *sly-db-quit-restart*)))))

(defslyfun invoke-nth-restart-for-emacs (sly-db-level n)
  "Invoke the Nth available restart.
SLY-DB-LEVEL is the debug level when the request was made. If this
has changed, ignore the request."
  (when (= sly-db-level *sly-db-level*)
    (invoke-nth-restart n)))

(defun wrap-sly-db-vars (form)
  `(let ((*sly-db-level* ,*sly-db-level*))
     ,form))

(defun eval-in-frame-aux (frame string package print)
  (let* ((form (wrap-sly-db-vars (parse-string string package)))
         (values (multiple-value-list (eval-in-frame form frame))))
    (with-buffer-syntax (package)
      (funcall print values))))

(defslyfun eval-string-in-frame (string frame package)
  (eval-in-frame-aux frame string package #'echo-for-emacs))

(defslyfun pprint-eval-string-in-frame (string frame package)
  (eval-in-frame-aux frame string package #'slynk-pprint-values))

(defslyfun frame-package-name (frame)
  (let ((pkg (frame-package frame)))
    (cond (pkg (package-name pkg))
          (t (with-buffer-syntax () (package-name *package*))))))

(defslyfun frame-locals-and-catch-tags (index)
  "Return a list (LOCALS TAGS) for vars and catch tags in the frame INDEX.
LOCALS is a list of the form ((&key NAME ID VALUE) ...).
TAGS has is a list of strings."
  (list (frame-locals-for-emacs index)
        (mapcar #'to-string (frame-catch-tags index))))

(defun frame-locals-for-emacs (index)
  (with-bindings *backtrace-printer-bindings*
    (loop for var in (frame-locals index) collect
          (destructuring-bind (&key name id value) var
            (list :name (let ((*package* (or (frame-package index) *package*)))
                          (prin1-to-string name))
                  :id id
                  :value (slynk-pprint-to-line value *print-right-margin*))))))

(defslyfun sly-db-disassemble (index)
  (with-output-to-string (*standard-output*)
    (disassemble-frame index)))

(defslyfun sly-db-return-from-frame (index string)
  (let ((form (from-string string)))
    (to-string (multiple-value-list (return-from-frame index form)))))

(defslyfun sly-db-break (name)
  (with-buffer-syntax ()
    (sly-db-break-at-start (read-from-string name))))

(defmacro define-stepper-function (name backend-function-name)
  `(defslyfun ,name (frame)
     (cond ((sly-db-stepper-condition-p *slynk-debugger-condition*)
            (setq *sly-db-stepping-p* t)
            (,backend-function-name))
           ((find-restart 'continue)
            (activate-stepping frame)
            (setq *sly-db-stepping-p* t)
            (continue))
           (t
            (error "Not currently single-stepping, ~
and no continue restart available.")))))

(define-stepper-function sly-db-step sly-db-step-into)
(define-stepper-function sly-db-next sly-db-step-next)
(define-stepper-function sly-db-out  sly-db-step-out)

(defslyfun toggle-break-on-signals ()
  (setq *break-on-signals* (not *break-on-signals*))
  (format nil "*break-on-signals* = ~a" *break-on-signals*))

(defslyfun sdlb-print-condition ()
  (princ-to-string *slynk-debugger-condition*))
