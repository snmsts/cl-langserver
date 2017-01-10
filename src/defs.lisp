(defpackage :langserver-base
  (:use :cl :ls-backend)
  (:export #:startup-multiprocessing
           #:start-server
           #:create-server
           #:stop-server
           #:restart-server
           #:ed-in-emacs
           #:inspect-in-emacs
           #:print-indentation-lossage
           #:invoke-sly-debugger
           #:slynk-debugger-hook
           #:emacs-inspect
           ;;#:inspect-slot-for-emacs
           #:authenticate-client
           #:*loopback-interface*
           #:*buffer-readtable*
           #:*exclude-symbol-functions*)
  ;; These are user-configurable variables:
  (:export #:*communication-style*
           #:*dont-close*
           #:*fasl-pathname-function*
           #:*log-events*
           #:*log-output*
           #:*configure-emacs-indentation*
           #:*readtable-alist*
           #:*global-debugger*
           #:*sly-db-quit-restart*
           #:*backtrace-printer-bindings*
           #:*default-worker-thread-bindings*
           #:*macroexpand-printer-bindings*
           #:*slynk-pprint-bindings*
           #:*inspector-verbose*
           #:*require-module*
           #:*eval-for-emacs-wrappers*
           #:*debugger-extra-options*
           ;; This is SETFable.
           #:debug-on-slynk-error
           ;; These are re-exported directly from the backend:
           #:buffer-first-change
           #:frame-source-location
           #:gdb-initial-commands
           #:restart-frame
           #:sly-db-step
           #:sly-db-break
           #:sly-db-break-on-return
           #:default-directory
           #:set-default-directory
           #:quit-lisp
           #:eval-for-emacs
           #:eval-in-emacs
           #:y-or-n-p-in-emacs
           #:*find-definitions-right-trim*
           #:*find-definitions-left-trim*
           #:*after-toggle-trace-hook*
           #:*echo-number-alist*
           #:*present-number-alist*))

(in-package :langserver-base)


;;;; Top-level variables, constants, macros

(defconstant cl-package (find-package :cl)
  "The COMMON-LISP package.")

(defconstant keyword-package (find-package :keyword)
  "The KEYWORD package.")

(defconstant default-server-port 4005
  "The default TCP port for the server (when started manually).")

(defvar *langserver-debug-p* t
  "When true, print extra debugging information.")

(defvar *backtrace-pprint-dispatch-table*
  (let ((table (copy-pprint-dispatch nil)))
    (flet ((print-string (stream string)
             (cond (*print-escape*
                    (escape-string string stream
                                   :map '((#\" . "\\\"")
                                          (#\\ . "\\\\")
                                          (#\newline . "\\n")
                                          (#\return . "\\r"))))
                   (t (write-string string stream)))))
      (set-pprint-dispatch 'string  #'print-string 0 table)
      table)))

(defvar *backtrace-printer-bindings*
  `((*print-pretty*           . t)
    (*print-readably*         . nil)
    (*print-level*            . 4)
    (*print-length*           . 6)
    (*print-lines*            . 1)
    (*print-right-margin*     . 200)
    (*print-pprint-dispatch*  . ,*backtrace-pprint-dispatch-table*))
  "Pretter settings for printing backtraces.")

(defvar *default-worker-thread-bindings* '()
  "An alist to initialize dynamic variables in worker threads.
The list has the form ((VAR . VALUE) ...).  Each variable VAR will be
bound to the corresponding VALUE.")

(defun call-with-bindings (alist fun)
  "Call FUN with variables bound according to ALIST.
ALIST is a list of the form ((VAR . VAL) ...)."
  (if (null alist)
      (funcall fun)
      (let* ((rlist (reverse alist))
             (vars (mapcar #'car rlist))
             (vals (mapcar #'cdr rlist)))
        (progv vars vals
          (funcall fun)))))

(defmacro with-bindings (alist &body body)
  "See `call-with-bindings'.
Bindings appearing earlier in the list take priority"
  `(call-with-bindings ,alist (lambda () ,@body)))

;;; The `DEFRPCFUN' macro defines a function that client can call via
;;; RPC.

(defmacro defrpcfun (name arglist &body rest)
  "A DEFUN for functions that Emacs can call by RPC."
  `(progn
     (defun ,name ,arglist ,@rest)
     ;; see <http://www.franz.com/support/documentation/6.2/\
     ;; doc/pages/variables/compiler/\
     ;; s_cltl1-compile-file-toplevel-compatibility-p_s.htm>
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name (symbol-package ',name)))))

(defun missing-arg ()
  "A function that the compiler knows will never to return a value.
You can use (MISSING-ARG) as the initform for defstruct slots that
must always be supplied. This way the :TYPE slot option need not
include some arbitrary initial value like NIL."
  (error "A required &KEY or &OPTIONAL argument was not supplied."))

;;;; Utilities

;; stolen from Hunchentoot
(defmacro defvar-unbound (name &optional (doc-string ""))
  "Convenience macro to declare unbound special variables with a
documentation string."
  `(progn
     (defvar ,name)
     (setf (documentation ',name 'variable) ,doc-string)
     ',name))

;;;;; Helper macros

(defmacro destructure-case (value &body patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect
                 (if (eq pattern t)
                     `(t ,@body)
                     (destructuring-bind (op &rest rands) pattern
                       `(,op (destructuring-bind ,rands ,operands
                               ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))

(defvar *client-connection* nil
  "The connection to client currently in use.")
