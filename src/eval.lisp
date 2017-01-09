(in-package :ls-base)
;;;; Evaluation

(defvar *pending-continuations* '()
  "List of continuations for Emacs. (thread local)")

(defun guess-buffer-package (string)
  "Return a package for STRING.
Fall back to the current if no such package exists."
  (or (and string (guess-package string))
      *package*))

(defvar *eval-for-emacs-wrappers* nil
  "List of functions for fine-grained control over form evaluation.
Each element must be a function taking an arbitrary number of
arguments, the first of which is a function of no arguments, call it
IN-FUNCTION, while the remaining are bound to the EXTRA-REX-OPTIONS
parameter of EVAL-FOR-EMACS.  Every function *must* return another
function of no arguments, call it OUT-FUNCTION, that, when called,
*must* call IN-FUNCTION in whatever dynamic environment it sees fit.

Slynk will go through the elements of this variable in order, passing
a function that evaluates the form coming from Emacs to the first
element until it collects the result of the last, which is finally
called with no arguments.

Be careful when changing this variable since you may mess very basic
functionality of your Slynk, including the ability to correct any
errors you make.")

(defun eval-for-emacs (form buffer-package id &rest extra-rex-options)
  "Bind *BUFFER-PACKAGE* to BUFFER-PACKAGE and evaluate FORM.
Return the result to the continuation ID.  Errors are trapped and
invoke our debugger.  EXTRA-REX-OPTIONS are passed to the functions of
*EVAL-FOR-EMACS-WRAPPERS*, which see."
  (let (ok result condition)
    (unwind-protect
         (let ((*buffer-package* (guess-buffer-package buffer-package))
               (*buffer-readtable* (guess-buffer-readtable buffer-package))
               (*pending-continuations* (cons id *pending-continuations*)))
           (check-type *buffer-package* package)
           (check-type *buffer-readtable* readtable)
           (handler-bind ((t (lambda (c) (setf condition c))))
             (setq result (with-sly-interrupts
                            (flet ((eval-it ()
                                     ;; APPLY would be cleaner than EVAL.
                                     ;; (setq result (apply (car form) (cdr form)))
                                     (eval form)))
                              ;; Honour *EVAL-FOR-EMACS-WRAPPERS*
                              ;; 
                              (loop for lambda = #'eval-it then
                                                           (handler-case
                                                               (apply wrapper lambda extra-rex-options)
                                                             (error (e)
                                                               (warn "~s ignoring wrapper ~a (~a)"
                                                                     'eval-for-emacs wrapper e)
                                                               lambda))
                                    for wrapper in *eval-for-emacs-wrappers*
                                    finally (return (funcall lambda)))))))
           (run-hook *pre-reply-hook*)
           (setq ok t))
      (send-to-emacs `(:return ,(current-thread)
                               ,(if ok
                                    `(:ok ,result)
                                    `(:abort ,(prin1-to-string condition)))
                               ,id)))))

(defun format-integer-length (i) (format nil "~a bit~:p" (integer-length i)))
(defun format-integer-as-hex (i)
  (unless (or (minusp i) (> (integer-length i) 64)) (format nil "#x~X" i)))
(defun format-integer-as-octal (i)
  (unless (or (minusp i) (> (integer-length i) 8)) (format nil "#o~O" i)))
(defun format-integer-as-binary (i) -128
  (unless (or (minusp i) (> (integer-length i) 8)) (format nil "#b~B" i)))
(defun format-ratio-as-float (r) (ignore-errors (format nil "~f" r)))
(defun format-as-percentage-maybe (f) (when (< 0 (abs f) 2) (format nil "~2,'0d%" (* f 100))))

(defparameter *echo-number-alist*
  '((integer . (format-integer-length format-integer-as-hex format-integer-as-octal format-integer-as-binary))
    (ratio . (format-ratio-as-float format-as-percentage-maybe))
    (float . (format-as-percentage-maybe)))
  "Alist of functions used for presenting numbers in the echo area.

Each element takes the form (TYPE . FUNCTIONS), where TYPE is a type
designator and FUNCTIONS is a list of function designators for
displaying that number in SLY. Each function takes the number as a
single argument and returns a string, or nil, if that particular
representation is to be disregarded.

Additionally if a given function chooses to return t as its optional
second value, then all the remaining functions following it in the
list are disregarded.")

(defparameter *present-number-alist* nil
  "Alist of functions used for presenting numbers the REPL.

This is an \"override\". If nil the (the alist is empty) the value of
*ECHO-NUMBER-ALIST* is used, otherwise the structure is exactly the
same as that variable.")

(defun present-number-considering-alist (number alist)
  (let* ((functions (cdr (assoc number alist :test #'typep)))
         (extra-presentations
           (loop for fn in functions
                 for (display skip)
                   = (multiple-value-list
                      (handler-case
                          (funcall fn number)
                        (error (e)
                          (declare (ignore e))
                          "<error echoing>")))
                 when display collect it
                   until skip)))
    (if extra-presentations
        (format nil "~A (~{~a~^, ~})"
                number extra-presentations)
        (format nil "~A" number))))

(defun echo-for-emacs (values)
  "Format VALUES in a way suitable to be echoed in the SLY client"
  (let ((*print-readably* nil))
    (cond ((null values) "; No value")
          ((and (numberp (car values))
                (null (cdr values)))
           (present-number-considering-alist (car values) *echo-number-alist*))
          (t
           (let ((strings (loop for v in values
                                collect (slynk-pprint-to-line v))))
             (if (some #'(lambda (s) (find #\Newline s))
                       strings)
                 (format nil "~{~a~^~%~}" strings)
                 (format nil "~{~a~^, ~}" strings)))))))

(defun present-for-emacs (value &optional (fn #'slynk-pprint))
  "Format VALUE in a way suitable to be displayed in the SLY client.
FN is only used if value is not a number"
  (if (numberp value)
      (present-number-considering-alist value (or *present-number-alist*
                                                  *echo-number-alist*))
      (funcall fn value)))

(defslyfun interactive-eval (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY interactive evaluation request.")
      (let ((values (multiple-value-list (eval (from-string string)))))
        (finish-output)
        (echo-for-emacs values)))))

(defslyfun eval-and-grab-output (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY evaluation request.")
      (let* ((s (make-string-output-stream))
             (*standard-output* s)
             (values (multiple-value-list (eval (from-string string)))))
        (list (get-output-stream-string s)
              (echo-for-emacs values))))))

(defun eval-region (string)
  "Evaluate STRING.
Return the results of the last form as a list and as secondary value the
last form."
  (with-input-from-string (stream string)
    (let (- values)
      (loop
       (let ((form (read stream nil stream)))
         (when (eq form stream)
           (finish-output)
           (return (values values -)))
         (setq - form)
         (setq values (multiple-value-list (eval form)))
         (finish-output))))))

(defslyfun interactive-eval-region (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY interactive evaluation request.")
      (echo-for-emacs (eval-region string)))))

(defslyfun re-evaluate-defvar (form)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY evaluation request.")
      (let ((form (read-from-string form)))
        (destructuring-bind (dv name &optional value doc) form
          (declare (ignore value doc))
          (assert (eq dv 'defvar))
          (makunbound name)
          (prin1-to-string (eval form)))))))

(defparameter *slynk-pprint-bindings*
  `((*print-pretty*   . t)
    (*print-level*    . nil)
    (*print-length*   . nil)
    (*print-circle*   . nil)
    (*print-gensym*   . t)
    (*print-readably* . nil))
  "A list of variables bindings during pretty printing.
Used by pprint-eval.")

(defun slynk-pprint (object &key (stream nil))
  "Pretty print OBJECT to STREAM using *SLYNK-PPRINT-BINDINGS*.
If STREAM is nil, use a string"
  (with-bindings *slynk-pprint-bindings*
    ;; a failsafe for *PRINT-LENGTH*: if it's NIL and *PRINT-CIRCLE*
    ;; is also nil we could be in trouble printing circular lists, for example.
    ;; 
    (let ((*print-length* (if (and (not *print-circle*)
                                   (not *print-length*))
                              512
                              *print-length*)))
      (without-printing-errors (:object object :stream stream)
        (if stream
            (write object :stream stream :pretty t :escape t)
            (with-output-to-string (s)
              (slynk-pprint object :stream s)))))))

(defun slynk-pprint-values (values &key (stream nil))
  "Pretty print each of VALUES to STREAM using *SLYNK-PPRINT-BINDINGS*.
Separated by a newline. If no values indicate that in a comment.
If STREAM is nil, use a string"
  (labels ((print-one (object s)
             (let ((*slynk-pprint-bindings* nil))
               (slynk-pprint object :stream s)))
           (print-all (s)
             (loop for o in values
                   do (print-one o s)
                      (terpri))))
    (with-bindings *slynk-pprint-bindings*
      (cond ((null values)
             (format stream "; No value"))
            (t
             (if stream
                 (print-all stream)
                 (with-output-to-string (s)
                   (print-all s))))))))

(defun slynk-pprint-to-line (object &optional width)
  "Print OBJECT to a single line at most. Return the string."
  (let ((*slynk-pprint-bindings*
          `((*print-right-margin* . ,(or width 512))
            (*print-lines* . 1)
            ,@*slynk-pprint-bindings*)))
    (slynk-pprint object)))

(defslyfun pprint-eval (string)
  (with-buffer-syntax ()
    (let* ((s (make-string-output-stream))
           (values
            (let ((*standard-output* s)
                  (*trace-output* s))
              (multiple-value-list (eval (read-from-string string))))))
      (cat (get-output-stream-string s)
           (slynk-pprint-values values)))))

(defslyfun set-package (name)
  "Set *package* to the package named NAME.
Return the full package-name and the string to use in the prompt."
  (let ((p (guess-package name)))
    (assert (packagep p) nil "Package ~a doesn't exist." name)
    (setq *package* p)
    (list (package-name p) (package-string-for-prompt p))))

(defun cat (&rest strings)
  "Concatenate all arguments and make the result a string."
  (with-output-to-string (out)
    (dolist (s strings)
      (etypecase s
        (string (write-string s out))
        (character (write-char s out))))))

(defun truncate-string (string width &optional ellipsis)
  (let ((len (length string)))
    (cond ((< len width) string)
          (ellipsis (cat (subseq string 0 width) ellipsis))
          (t (subseq string 0 width)))))

(defun call/truncated-output-to-string (length function
                                        &optional (ellipsis ".."))
  "Call FUNCTION with a new stream, return the output written to the stream.
If FUNCTION tries to write more than LENGTH characters, it will be
aborted and return immediately with the output written so far."
  (let ((buffer (make-string (+ length (length ellipsis))))
        (fill-pointer 0))
    (block buffer-full
      (flet ((write-output (string)
               (let* ((free (- length fill-pointer))
                      (count (min free (length string))))
                 (replace buffer string :start1 fill-pointer :end2 count)
                 (incf fill-pointer count)
                 (when (> (length string) free)
                   (replace buffer ellipsis :start1 fill-pointer)
                   (return-from buffer-full buffer)))))
        (let ((stream (make-output-stream #'write-output)))
          (funcall function stream)
          (finish-output stream)
          (subseq buffer 0 fill-pointer))))))

(defmacro with-string-stream ((var &key length bindings)
                              &body body)
  (cond ((and (not bindings) (not length))
         `(with-output-to-string (,var) . ,body))
        ((not bindings)
         `(call/truncated-output-to-string
           ,length (lambda (,var) . ,body)))
        (t
         `(with-bindings ,bindings
            (with-string-stream (,var :length ,length)
              . ,body)))))

(defun escape-string (string stream &key length (map '((#\" . "\\\"")
                                                       (#\\ . "\\\\"))))
  "Write STRING to STREAM surronded by double-quotes.
LENGTH -- if non-nil truncate output after LENGTH chars.
MAP -- rewrite the chars in STRING according to this alist."
  (let ((limit (or length array-dimension-limit)))
    (write-char #\" stream)
    (loop for c across string
          for i from 0 do
          (when (= i limit)
            (write-string "..." stream)
            (return))
          (let ((probe (assoc c map)))
            (cond (probe (write-string (cdr probe) stream))
                  (t (write-char c stream)))))
    (write-char #\" stream)))
