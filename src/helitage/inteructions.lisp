(in-package :ls-base)

(defvar *sly-features* nil
  "The feature list that has been sent to Emacs.")

(defun send-oob-to-emacs (object)
  (send-to-emacs object))

(defun force-user-output ()
  (with-default-listener (*emacs-connection*)
    (force-output *standard-output*)))

(add-hook *pre-reply-hook* 'force-user-output)

(defun clear-user-input  ()
  (with-default-listener (*emacs-connection*)
    (clear-input *standard-input*)))

;; FIXME: not thread safe.
(defvar *tag-counter* 0)

(defun make-tag ()
  (setq *tag-counter* (mod (1+ *tag-counter*) (expt 2 22))))

(defun y-or-n-p-in-emacs (format-string &rest arguments)
  "Like y-or-n-p, but ask in the Emacs minibuffer."
  (let ((tag (make-tag))
        (question (apply #'format nil format-string arguments)))
    (force-output)
    (send-to-emacs `(:y-or-n-p ,(current-thread-id) ,tag ,question))
    (third (wait-for-event `(:emacs-return ,tag result)))))

(defun read-from-minibuffer-in-emacs (prompt &optional initial-value)
  "Ask user a question in Emacs' minibuffer. Returns \"\" when user
entered nothing, returns NIL when user pressed C-g."
  (check-type prompt string) (check-type initial-value (or null string))
  (let ((tag (make-tag)))
    (force-output)
    (send-to-emacs `(:read-from-minibuffer ,(current-thread-id) ,tag
                                           ,prompt ,initial-value))
    (third (wait-for-event `(:emacs-return ,tag result)))))

(defun process-form-for-emacs (form)
  "Returns a string which emacs will read as equivalent to
FORM. FORM can contain lists, strings, characters, symbols and
numbers.

Characters are converted emacs' ?<char> notaion, strings are left
as they are (except for espacing any nested \" chars, numbers are
printed in base 10 and symbols are printed as their symbol-name
converted to lower case."
  (etypecase form
    (string (format nil "~S" form))
    (cons (format nil "(~A . ~A)"
                  (process-form-for-emacs (car form))
                  (process-form-for-emacs (cdr form))))
    (character (format nil "?~C" form))
    (symbol (concatenate 'string (when (eq (symbol-package form)
                                           #.(find-package "KEYWORD"))
                                   ":")
                         (string-downcase (symbol-name form))))
    (number (let ((*print-base* 10))
              (princ-to-string form)))))

(defun eval-in-emacs (form &optional nowait)
  "Eval FORM in Emacs.
`sly-enable-evaluate-in-emacs' should be set to T on the Emacs side."
  (cond (nowait
         (send-to-emacs `(:eval-no-wait ,(process-form-for-emacs form))))
        (t
         (force-output)
         (let ((tag (make-tag)))
	   (send-to-emacs `(:eval ,(current-thread-id) ,tag
				  ,(process-form-for-emacs form)))
	   (let ((value (caddr (wait-for-event `(:emacs-return ,tag result)))))
	     (destructure-case value
	       ((:ok value) value)
               ((:error kind . data) (error "~a: ~{~a~}" kind data))
	       ((:abort) (abort))))))))

(defun sly-version-string ()
  "Return a string identifying the SLY version.
Return nil if nothing appropriate is available."
  (let ((this-file #.(or *compile-file-truename* *load-truename*)))
    (with-open-file (s (make-pathname :name "sly" :type "el"
                                      :directory (butlast
                                                  (pathname-directory this-file)
                                                  1)
                                      :defaults this-file))
      (let ((seq (make-array 200 :element-type 'character :initial-element #\null)))
        (read-sequence seq s :end 200)
        (let* ((beg (search ";; Version:" seq))
               (end (position #\NewLine seq :start beg))
               (middle (position #\Space seq :from-end t :end end)))
          (subseq seq (1+ middle) end))))))

(defvar *slynk-wire-protocol-version* (ignore-errors (sly-version-string))
  "The version of the slynk/sly communication protocol.")

(defslyfun connection-info ()
  "Return a key-value list of the form:
\(&key PID STYLE LISP-IMPLEMENTATION MACHINE FEATURES PACKAGE VERSION)
PID: is the process-id of Lisp process (or nil, depending on the STYLE)
STYLE: the communication style
LISP-IMPLEMENTATION: a list (&key TYPE NAME VERSION PROGRAM)
FEATURES: a list of keywords
PACKAGE: a list (&key NAME PROMPT)
VERSION: the protocol version"
  (let ((c *emacs-connection*))
    (setq *sly-features* *features*)
    `(:pid ,(getpid) :style ,(connection-communication-style c)
      :encoding (:coding-systems
                 ,(loop for cs in '("utf-8-unix" "iso-latin-1-unix")
                        when (find-external-format cs) collect cs))
      :lisp-implementation (:type ,(lisp-implementation-type)
                            :name ,(lisp-implementation-type-name)
                            :version ,(lisp-implementation-version)
                            :program ,(lisp-implementation-program))
      :machine (:instance ,(machine-instance)
               :type ,(machine-type)
               :version ,(machine-version))
      :features ,(features-for-emacs)
      :modules ,*modules*
      :package (:name ,(package-name *package*)
               :prompt ,(package-string-for-prompt *package*))
      :version ,*slynk-wire-protocol-version*)))

(defun debug-on-slynk-error ()
  (assert (eq *debug-on-slynk-protocol-error* *debug-ls-backend*))
  *debug-on-slynk-protocol-error*)

(defun (setf debug-on-slynk-error) (new-value)
  (setf *debug-on-slynk-protocol-error* new-value)
  (setf *debug-ls-backend* new-value))

(defslyfun toggle-debug-on-slynk-error ()
  (setf (debug-on-slynk-error) (not (debug-on-slynk-error))))
