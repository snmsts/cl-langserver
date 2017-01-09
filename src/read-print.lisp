(in-package :ls-base)
;;;; Reading and printing

(defvar-unbound *buffer-package*
    "Package corresponding to sly-buffer-package.

EVAL-FOR-EMACS binds *buffer-package*.  Strings originating from a sly
buffer are best read in this package.  See also FROM-STRING and TO-STRING.")

(defvar-unbound *buffer-readtable*
    "Readtable associated with the current buffer")

(defmacro with-buffer-syntax ((&optional package-designator
                                         readtable)
                              &body body)
  "Conceptually execute BODY inside a SLY Lisp buffer.

Execute BODY with appropriate *PACKAGE* and *READTABLE* bindings.

PACKAGE-DESIGNATOR, if non-NIL, is anything remotely designating a
package.  READTABLE, if non-NIL, must verify CL:READTABLEP.

READTABLE defaults to *BUFFER-READTABLE* as set by
GUESS-BUFFER-READTABLE, which in turn uses a mapping in
*READTABLE-ALIST* as indexed by *BUFFER-PACKAGE*, and *not*
PACKAGE-DESIGNATOR.

This should be used for code that is conceptionally executed in an
Emacs buffer."
  `(call-with-buffer-syntax ,package-designator ,readtable (lambda () ,@body)))

(defun call-with-buffer-syntax (package readtable fun)
  (let ((*package* (if package
                       (guess-buffer-package package)
                       *buffer-package*))
        (*buffer-readtable* (or (and (readtablep readtable)
                                     readtable)
                                *buffer-readtable*)))
    ;; Don't shadow *readtable* unnecessarily because that prevents
    ;; the user from assigning to it.
    (if (eq *readtable* *buffer-readtable*)
        (call-with-syntax-hooks fun)
        (let ((*readtable* *buffer-readtable*))
          (call-with-syntax-hooks fun)))))

(defmacro without-printing-errors ((&key object stream
                                        (msg "<<error printing object>>"))
                                  &body body)
  "Catches errors during evaluation of BODY and prints MSG instead."
  `(handler-case (progn ,@body)
     (serious-condition ()
       ,(cond ((and stream object)
               (let ((gstream (gensym "STREAM+")))
                 `(let ((,gstream ,stream))
                    (print-unreadable-object (,object ,gstream :type t
                                                      :identity t)
                      (write-string ,msg ,gstream)))))
              (stream
               `(write-string ,msg ,stream))
              (object
               `(with-output-to-string (s)
                  (print-unreadable-object (,object s :type t :identity t)
                    (write-string ,msg  s))))
              (t msg)))))

(defun to-string (object)
  "Write OBJECT in the *BUFFER-PACKAGE*.
The result may not be readable. Handles problems with PRINT-OBJECT methods
gracefully."
  (with-buffer-syntax ()
    (let ((*print-readably* nil))
      (without-printing-errors (:object object :stream nil)
        (prin1-to-string object)))))

(defun from-string (string)
  "Read string in the *BUFFER-PACKAGE*"
  (with-buffer-syntax ()
    (let ((*read-suppress* nil))
      (values (read-from-string string)))))

(defun parse-string (string package)
  "Read STRING in PACKAGE."
  (with-buffer-syntax (package)
    (let ((*read-suppress* nil))
      (read-from-string string))))

;; FIXME: deal with #\| etc.  hard to do portably.
(defun tokenize-symbol (string)
  "STRING is interpreted as the string representation of a symbol
and is tokenized accordingly. The result is returned in three
values: The package identifier part, the actual symbol identifier
part, and a flag if the STRING represents a symbol that is
internal to the package identifier part. (Notice that the flag is
also true with an empty package identifier part, as the STRING is
considered to represent a symbol internal to some current package.)"
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (not (= (count #\: string) 1))))
    (values symbol package internp)))

(defun tokenize-symbol-thoroughly (string)
  "This version of TOKENIZE-SYMBOL handles escape characters."
  (let ((package nil)
        (token (make-array (length string) :element-type 'character
                           :fill-pointer 0))
        (backslash nil)
        (vertical nil)
        (internp nil))
    (loop for char across string do
          (cond
            (backslash
             (vector-push-extend char token)
             (setq backslash nil))
            ((char= char #\\) ; Quotes next character, even within |...|
             (setq backslash t))
            ((char= char #\|)
             (setq vertical (not vertical)))
            (vertical
             (vector-push-extend char token))
            ((char= char #\:)
             (cond ((and package internp)
                    (return-from tokenize-symbol-thoroughly))
                   (package
                    (setq internp t))
                   (t
                    (setq package token
                          token (make-array (length string)
                                            :element-type 'character
                                            :fill-pointer 0)))))
            (t
             (vector-push-extend (casify-char char) token))))
    (unless vertical
          (values token package (or (not package) internp)))))

(defun untokenize-symbol (package-name internal-p symbol-name)
  "The inverse of TOKENIZE-SYMBOL.

  (untokenize-symbol \"quux\" nil \"foo\") ==> \"quux:foo\"
  (untokenize-symbol \"quux\" t \"foo\")   ==> \"quux::foo\"
  (untokenize-symbol nil nil \"foo\")    ==> \"foo\"
"
  (cond ((not package-name) 	symbol-name)
        (internal-p 		(cat package-name "::" symbol-name))
        (t 			(cat package-name ":" symbol-name))))

(defun casify-char (char)
  "Convert CHAR accoring to readtable-case."
  (ecase (readtable-case *readtable*)
    (:preserve char)
    (:upcase   (char-upcase char))
    (:downcase (char-downcase char))
    (:invert (if (upper-case-p char)
                 (char-downcase char)
                 (char-upcase char)))))


(defun find-symbol-with-status (symbol-name status
                                &optional (package *package*))
  (multiple-value-bind (symbol flag) (find-symbol symbol-name package)
    (if (and flag (eq flag status))
        (values symbol flag)
        (values nil nil))))

(defun parse-symbol (string &optional (package *package*))
  "Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbols was found."
  (multiple-value-bind (sname pname internalp)
      (tokenize-symbol-thoroughly string)
    (when sname
     (let ((package (cond ((string= pname "") keyword-package)
                          (pname              (find-package pname))
                          (t                  package))))
       (if package
           (multiple-value-bind (symbol flag)
               (if internalp
                   (find-symbol sname package)
                   (find-symbol-with-status sname ':external package))
             (values symbol flag sname package))
           (values nil nil nil nil))))))

(defun parse-symbol-or-lose (string &optional (package *package*))
  (multiple-value-bind (symbol status) (parse-symbol string package)
    (if status
        (values symbol status)
        (error "Unknown symbol: ~A [in ~A]" string package))))

(defun parse-package (string)
  "Find the package named STRING.
Return the package or nil."
  ;; STRING comes usually from a (in-package STRING) form.
  (ignore-errors
    (find-package (let ((*package* *slynk-io-package*))
                    (read-from-string string)))))

(defun unparse-name (string)
  "Print the name STRING according to the current printer settings."
  ;; this is intended for package or symbol names
  (subseq (prin1-to-string (make-symbol string)) 2))

(defun guess-package (string)
  "Guess which package corresponds to STRING.
Return nil if no package matches."
  (when string
    (or (find-package string)
        (parse-package string)
        (if (find #\! string)           ; for SBCL
            (guess-package (substitute #\- #\! string))))))

(defvar *readtable-alist* (default-readtable-alist)
  "An alist mapping package names to readtables.")

(defun guess-buffer-readtable (package-name)
  (let ((package (guess-package package-name)))
    (or (and package
             (cdr (assoc (package-name package) *readtable-alist*
                         :test #'string=)))
        *readtable*)))
