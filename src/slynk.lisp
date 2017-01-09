(in-package :ls-base)


;;;; Simple arglist display

(defslyfun operator-arglist (name package)
  (ignore-errors
    (let ((args (arglist (parse-symbol name (guess-buffer-package package)))))
      (cond ((eq args :not-available) nil)
	    (t (princ-to-string (cons name args)))))))


;;;; Documentation

(defslyfun apropos-list-for-emacs  (name &optional external-only
                                         case-sensitive package)
  "Make an apropos search for Emacs.
The result is a list of property lists."
  (let ((package (if package
                     (or (parse-package package)
                         (error "No such package: ~S" package)))))
    ;; The MAPCAN will filter all uninteresting symbols, i.e. those
    ;; who cannot be meaningfully described.
    ;;
    ;; *BUFFER-PACKAGE* is exceptionally set so that the symbol
    ;; listing will only omit package qualifier iff the user specified
    ;; PACKAGE.
    (let ((*buffer-package* (or package
                                *slynk-io-package*)))
      (loop for (symbol . extra)
              in (sort (remove-duplicates
                        (apropos-symbols name external-only case-sensitive package)
                        :key #'first)
                       #'present-symbol-before-p
                       :key #'first)
            for short = (briefly-describe-symbol-for-emacs symbol)
            when short
              collect (append short extra)))))

(defun briefly-describe-symbol-for-emacs (symbol)
  "Return a property list describing SYMBOL.
Like `describe-symbol-for-emacs' but with at most one line per item."
  (flet ((first-line (string)
           (let ((pos (position #\newline string)))
             (if (null pos) string (subseq string 0 pos)))))
    (let ((desc (map-if #'stringp #'first-line
                        (describe-symbol-for-emacs symbol))))
      (if desc
          `(:designator ,(list (symbol-name symbol)
                               (let ((package (symbol-package symbol)))
                                 (and package
                                      (package-name package)))
                               (symbol-external-p symbol))
                        ,@desc
                        ,@(let ((arglist (and (fboundp symbol)
                                              (arglist symbol))))
                            (when (and arglist
                                       (not (eq arglist :not-available)))
                              `(:arglist ,(princ-to-string arglist)))))))))

(defun map-if (test fn &rest lists)
  "Like (mapcar FN . LISTS) but only call FN on objects satisfying TEST.
Example:
\(map-if #'oddp #'- '(1 2 3 4 5)) => (-1 2 -3 4 -5)"
  (apply #'mapcar
         (lambda (x) (if (funcall test x) (funcall fn x) x))
         lists))

(defun listify (f)
  "Return a function like F, but which returns any non-null value
wrapped in a list."
  (lambda (x)
    (let ((y (funcall f x)))
      (and y (list y)))))

(defun present-symbol-before-p (x y)
  "Return true if X belongs before Y in a printed summary of symbols.
Sorted alphabetically by package name and then symbol name, except
that symbols accessible in the current package go first."
  (declare (type symbol x y))
  (flet ((accessible (s)
           ;; Test breaks on NIL for package that does not inherit it
           (eq (find-symbol (symbol-name s) *buffer-package*) s)))
    (let ((ax (accessible x)) (ay (accessible y)))
      (cond ((and ax ay) (string< (symbol-name x) (symbol-name y)))
            (ax t)
            (ay nil)
            (t (let ((px (symbol-package x)) (py (symbol-package y)))
                 (if (eq px py)
                     (string< (symbol-name x) (symbol-name y))
                     (string< (package-name px) (package-name py)))))))))

(defun make-cl-ppcre-matcher (pattern case-sensitive symbol-name-fn)
  (let ((matcher (funcall (read-from-string "cl-ppcre:create-scanner")
                          pattern
                          :case-insensitive-mode (not case-sensitive))))
    (lambda (symbol)
      (funcall (read-from-string "cl-ppcre:scan")
               matcher
               (funcall symbol-name-fn symbol)))))

(defun make-plain-matcher (pattern case-sensitive symbol-name-fn)
  (let ((chr= (if case-sensitive #'char= #'char-equal)))
    (lambda (symbol)
      (let((beg (search pattern
                        (funcall symbol-name-fn symbol)
                        :test chr=)))
        (when beg
          (values beg (+ beg (length pattern))))))))

(defparameter *try-cl-ppcre-for-apropos* t
  "If non-NIL, maybe try CL-PPCRE for apropos requests.
CL-PPCRE must be loaded. This option has no effect if the
MAKE-APROPOS-MATCHER interface has been implemented.")

(defun apropos-symbols (pattern external-only case-sensitive package)
  (let* ((packages (or package (remove (find-package :keyword)
                                       (list-all-packages))))
         (symbol-name-fn
           (lambda (symbol)
             (cond ((not package)
                    ;; include qualifier in search if user didn't pass
                    ;; PACKAGE.
                    (concatenate 'string
                                 (package-name (symbol-package symbol))
                                 (if (symbol-external-p symbol) ":" "::")
                                 (symbol-name symbol)))
                   (t
                    (string symbol)))))
         (interface-unimplemented-p
           (find 'ls-backend:make-apropos-matcher
                 ls-backend::*unimplemented-interfaces*))
         (attempt-cl-ppcre (and *try-cl-ppcre-for-apropos*
                                (not (every #'alpha-char-p pattern))))
         (cl-ppcre-matcher (and attempt-cl-ppcre
                                (find-package :cl-ppcre)
                                (ignore-errors
                                 (make-cl-ppcre-matcher pattern case-sensitive symbol-name-fn))))
         (matcher (cond ((and interface-unimplemented-p
                              attempt-cl-ppcre
                              cl-ppcre-matcher)
                         ;; Use regexp apropos we guess the user has
                         ;; requested it and if it is possible.
                         ;;
                         (background-message "Using CL-PPCRE for apropos on regexp \"~a\"" pattern)
                         cl-ppcre-matcher)
                        (interface-unimplemented-p
                         ;; Use plain apropos otherwise
                         ;; 
                         (when attempt-cl-ppcre
                           (if (not (find-package :cl-ppcre))
                               (background-message "Using plain apropos. Load CL-PPCRE to enable regexps")
                               (background-message "Not a valid CL-PPCRE regexp, so using plain apropos")))
                         (make-plain-matcher pattern case-sensitive symbol-name-fn))
                        (t
                         (ls-backend:make-apropos-matcher pattern
                                                             symbol-name-fn
                                                             case-sensitive)))))
    (with-package-iterator (next packages :external :internal)
      (loop for (morep symbol) = (multiple-value-list (next))
            while morep
            for (match end) = (and (not (excluded-from-searches-p symbol))
                                   (or (not external-only)
                                       (symbol-external-p symbol))
                                   (symbol-package symbol)
                                   (multiple-value-list (funcall matcher symbol)))
            when match
              collect `(,symbol ,@(when end `(:bounds (,match ,end))))))))

(defun call-with-describe-settings (fn)
  (let ((*print-readably* nil))
    (funcall fn)))

(defmacro with-describe-settings ((&rest _) &body body)
  (declare (ignore _))
  `(call-with-describe-settings (lambda () ,@body)))

(defun describe-to-string (object)
  (with-describe-settings ()
    (with-output-to-string (*standard-output*)
      (describe object))))

(defslyfun describe-symbol (symbol-name)
  (with-buffer-syntax ()
    (describe-to-string (parse-symbol-or-lose symbol-name))))

(defslyfun describe-function (name)
  (with-buffer-syntax ()
    (let ((symbol (parse-symbol-or-lose name)))
      (describe-to-string (or (macro-function symbol)
                              (symbol-function symbol))))))

(defslyfun describe-definition-for-emacs (name kind)
  (with-buffer-syntax ()
    (with-describe-settings ()
      (with-output-to-string (*standard-output*)
        (describe-definition (parse-symbol-or-lose name) kind)))))

(defslyfun documentation-symbol (symbol-name)
  (with-buffer-syntax ()
    (multiple-value-bind (sym foundp) (parse-symbol symbol-name)
      (if foundp
          (let ((vdoc (documentation sym 'variable))
                (fdoc (documentation sym 'function)))
            (with-output-to-string (string)
              (format string "Documentation for the symbol ~a:~2%" sym)
              (unless (or vdoc fdoc)
                (format string "Not documented." ))
              (when vdoc
                (format string "Variable:~% ~a~2%" vdoc))
              (when fdoc
                (format string "Function:~% Arglist: ~a~2% ~a"
                        (ls-backend:arglist sym)
                        fdoc))))
          (format nil "No such symbol, ~a." symbol-name)))))


;;;; Package Commands

(defslyfun list-all-package-names (&optional nicknames)
  "Return a list of all package names.
Include the nicknames if NICKNAMES is true."
  (mapcar #'unparse-name
          (if nicknames
              (mapcan #'package-names (list-all-packages))
              (mapcar #'package-name  (list-all-packages)))))


;;;; Tracing

;; Use eval for the sake of portability...
(defun tracedp (fspec)
  (member fspec (eval '(trace))))

(defvar *after-toggle-trace-hook* nil
  "Hook called whenever a SPEC is traced or untraced.

If non-nil, called with two arguments SPEC and TRACED-P." )
(defslyfun slynk-toggle-trace (spec-string)
  (let* ((spec (from-string spec-string))
         (retval (cond ((consp spec) ; handle complicated cases in the backend
                        (toggle-trace spec))
                       ((tracedp spec)
                        (eval `(untrace ,spec))
                        (format nil "~S is now untraced." spec))
                       (t
                        (eval `(trace ,spec))
                        (format nil "~S is now traced." spec))))
         (traced-p (let* ((tosearch "is now traced.")
                          (start (- (length retval)
                                    (length tosearch)))
                          (end (+ start (length tosearch))))
                     (search tosearch (subseq retval start end))))
         (hook-msg (when *after-toggle-trace-hook*
                     (funcall *after-toggle-trace-hook*
                              spec
                              traced-p))))
    (if hook-msg
        (format nil "~a~%(also ~a)" retval hook-msg)
        retval)))

(defslyfun untrace-all ()
  (untrace))


;;;; Undefing

(defslyfun undefine-function (fname-string)
  (let ((fname (from-string fname-string)))
    (format nil "~S" (fmakunbound fname))))

(defslyfun unintern-symbol (name package)
  (let ((pkg (guess-package package)))
    (cond ((not pkg) (format nil "No such package: ~s" package))
          (t
           (multiple-value-bind (sym found) (parse-symbol name pkg)
             (case found
               ((nil) (format nil "~s not in package ~s" name package))
               (t
                (unintern sym pkg)
                (format nil "Uninterned symbol: ~s" sym))))))))

(defslyfun slynk-delete-package (package-name)
  (let ((pkg (or (guess-package package-name)
                 (error "No such package: ~s" package-name))))
    (delete-package pkg)
    nil))

;;;; Source Locations

(defslyfun find-definition-for-thing (thing)
  (find-source-location thing))

(defslyfun find-source-location-for-emacs (spec)
  (find-source-location (value-spec-ref spec)))

(defun value-spec-ref (spec)
  (destructure-case spec
    ((:string string package)
     (with-buffer-syntax (package)
       (eval (read-from-string string))))
    ((:inspector part)
     (inspector-nth-part part))
    ((:sly-db frame var)
     (frame-var-value frame var))))

(defvar *find-definitions-right-trim* ",:.>")
(defvar *find-definitions-left-trim* "#:<")

(defun find-definitions-find-symbol-or-package (name)
  (flet ((do-find (name)
           (multiple-value-bind (symbol found name)
               (with-buffer-syntax ()
                 (parse-symbol name))
             (cond (found
                    (return-from find-definitions-find-symbol-or-package
                      (values symbol found)))
                   ;; Packages are not named by symbols, so
                   ;; not-interned symbols can refer to packages
                   ((find-package name)
                    (return-from find-definitions-find-symbol-or-package
                      (values (make-symbol name) t)))))))
    (do-find name)
    (do-find (string-right-trim *find-definitions-right-trim* name))
    (do-find (string-left-trim *find-definitions-left-trim* name))
    (do-find (string-left-trim *find-definitions-left-trim*
                               (string-right-trim
                                *find-definitions-right-trim* name)))))

(defslyfun find-definitions-for-emacs (name)
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string."
  (multiple-value-bind (symbol found)
      (find-definitions-find-symbol-or-package name)
    (when found
      (mapcar #'xref>elisp (find-definitions symbol)))))

;;; Generic function so contribs can extend it.
(defgeneric xref-doit (type thing)
  (:method (type thing)
    (declare (ignore type thing))
    :not-implemented))

(macrolet ((define-xref-action (xref-type handler)
             `(defmethod xref-doit ((type (eql ,xref-type)) thing)
                (declare (ignorable type))
                (funcall ,handler thing))))
  (define-xref-action :calls        #'who-calls)
  (define-xref-action :calls-who    #'calls-who)
  (define-xref-action :references   #'who-references)
  (define-xref-action :binds        #'who-binds)
  (define-xref-action :sets         #'who-sets)
  (define-xref-action :macroexpands #'who-macroexpands)
  (define-xref-action :specializes  #'who-specializes)
  (define-xref-action :callers      #'list-callers)
  (define-xref-action :callees      #'list-callees))

(defslyfun xref (type name)
  (multiple-value-bind (sexp error) (ignore-errors (from-string name))
    (unless error
      (let ((xrefs  (xref-doit type sexp)))
        (if (eq xrefs :not-implemented)
            :not-implemented
            (mapcar #'xref>elisp xrefs))))))

(defslyfun xrefs (types name)
  (loop for type in types
        for xrefs = (xref type name)
        when (and (not (eq :not-implemented xrefs))
                  (not (null xrefs)))
          collect (cons type xrefs)))

(defun xref>elisp (xref)
  (destructuring-bind (name loc) xref
    (list (to-string name) loc)))


;;;;; Lazy lists

(defstruct (lcons (:constructor %lcons (car %cdr))
                  (:predicate lcons?))
  car
  (%cdr nil :type (or null lcons function))
  (forced? nil))

(defmacro lcons (car cdr)
  `(%lcons ,car (lambda () ,cdr)))

(defmacro lcons* (car cdr &rest more)
  (cond ((null more) `(lcons ,car ,cdr))
        (t `(lcons ,car (lcons* ,cdr ,@more)))))

(defun lcons-cdr (lcons)
  (with-struct* (lcons- @ lcons)
    (cond ((@ forced?)
           (@ %cdr))
          (t
           (let ((value (funcall (@ %cdr))))
             (setf (@ forced?) t
                   (@ %cdr) value))))))

(defun llist-range (llist start end)
  (llist-take (llist-skip llist start) (- end start)))

(defun llist-skip (lcons index)
  (do ((i 0 (1+ i))
       (l lcons (lcons-cdr l)))
      ((or (= i index) (null l))
       l)))

(defun llist-take (lcons count)
  (let ((result '()))
    (do ((i 0 (1+ i))
         (l lcons (lcons-cdr l)))
        ((or (= i count)
             (null l)))
      (push (lcons-car l) result))
    (nreverse result)))

(defun iline (label value)
  `(:line ,label ,value))


;;;; Inspecting
(defvar-unbound *current-inspector*
    "Current inspector, bound by EVAL-FOR-INSPECTOR, maybe to nil.")

(defvar-unbound *target-inspector*
    "Target inspector, bound by EVAL-FOR-INSPECTOR, maybe to nil.")

(defun current-inspector ()
  (or (and (boundp '*current-inspector*)
           *current-inspector*)
      (find-inspector "default")
      (make-instance 'inspector :name "default")))

(defun target-inspector ()
  (or (and (boundp '*target-inspector*)
           *target-inspector*)
      (current-inspector)))


(defvar *inspector-printer-bindings*
  '((*print-lines*        . 1)
    (*print-right-margin* . 75)
    (*print-pretty*       . t)
    (*print-readably*     . nil)))

(defvar *inspector-verbose-printer-bindings*
  '((*print-escape* . t)
    (*print-circle* . t)
    (*print-array*  . nil)))

(defclass inspector ()
  ((verbose-p :initform nil :accessor inspector-verbose-p)
   (history :initform (make-array 10 :adjustable t :fill-pointer 0) :accessor inspector-%history)
   (name :initarg :name :initform (error "Name this INSPECTOR!") :accessor inspector-name)))

(defmethod print-object ((i inspector) s)
  (print-unreadable-object (i s :type t) 
    (format s "~a/~a" (inspector-name i) (length (inspector-%history i)))))

(defmethod initialize-instance :after ((i inspector) &key name)
  (assert (not (find-inspector name)) nil "Already have an inspector named ~a" name)
  (push i (connection-inspectors *emacs-connection*)))

(defun find-inspector (name)
  (find name (connection-inspectors *emacs-connection*)
        :key #'inspector-name :test #'string=))

(defstruct inspector-state)
(defstruct (istate (:conc-name istate.) (:include inspector-state))
  object
  (parts (make-array 10 :adjustable t :fill-pointer 0))
  (actions (make-array 10 :adjustable t :fill-pointer 0))
  metadata
  content
  serial)

(defun ensure-istate-metadata (o indicator default)
  (with-struct (istate. object metadata) (current-istate)
    (assert (eq object o))
    (let ((data (getf metadata indicator default)))
      (setf (getf metadata indicator) data)
      data)))

(defun current-istate (&optional (inspector (current-inspector)))
  (let* ((history (inspector-%history inspector)))
    (and (plusp (length history))
         (aref history (1- (length history))))))

(defun reset-inspector ()
  (setf (inspector-%history (current-inspector))
        (make-array 10 :adjustable t :fill-pointer 0)))

(defslyfun init-inspector (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY inspection request.")
      (inspect-object (eval (read-from-string string))))))

(defun inspect-object (o)
  (let* ((inspector (target-inspector))
         (history (inspector-%history inspector))
         (istate (make-istate :object o)))
    (vector-push-extend istate history)
    (let ((*current-inspector* inspector))
      ;; HACK! because EMACS-INSPECT may call ENSURE-ISTATE-METADATA
      ;; which expects its object to be the current istate's objects.
      (setf (istate.content istate)
            (emacs-inspect o)))
    (vector-push-extend :break-history history)
    (decf (fill-pointer history))
    (istate>elisp istate)))

(defun istate>elisp (istate)
  (list :title (prepare-title istate)
        :id (assign-index (istate.object istate) (istate.parts istate))
        :content (prepare-range istate 0 500)
        ;; :serial (istate.serial istate)
        ))

(defun prepare-title (istate)
  (if (inspector-verbose-p (current-inspector))
      (with-bindings *inspector-verbose-printer-bindings*
        (to-string (istate.object istate)))
      (with-string-stream (stream :length 200
                                  :bindings *inspector-printer-bindings*)
        (print-unreadable-object
            ((istate.object istate) stream :type t :identity t)))))

(defun prepare-range (istate start end)
  (let* ((range (content-range (istate.content istate) start end))
         (ps (loop for part in range append (prepare-part part istate))))
    (list ps
          (if (< (length ps) (- end start))
              (+ start (length ps))
              (+ end 1000))
          start end)))

(defun prepare-part (part istate)
  (let ((newline '#.(string #\newline)))
    (etypecase part
      (string (list part))
      (cons (destructure-case part
              ((:newline) (list newline))
              ((:value obj &optional str)
               (list (value-part obj str (istate.parts istate))))
              ((:label &rest strs)
               (list (list :label (apply #'cat (mapcar #'string strs)))))
              ((:action label lambda &key (refreshp t))
               (list (action-part label lambda refreshp
                                  (istate.actions istate))))
              ((:line label value)
               (list (princ-to-string label) ": "
                     (value-part value nil (istate.parts istate))
                     newline)))))))

(defun value-part (object string parts)
  (list :value
        (or string (print-part-to-string object))
        (assign-index object parts)))

(defun action-part (label lambda refreshp actions)
  (list :action label (assign-index (list lambda refreshp) actions)))

(defun assign-index (object vector)
  (let ((index (fill-pointer vector)))
    (vector-push-extend object vector)
    index))

(defun print-part-to-string (value)
  (let* ((*print-readably* nil)
         (string (slynk-pprint-to-line value))
         (pos (position value
                        (inspector-%history (current-inspector))
                        :key #'istate.object)))
    (if pos
        (format nil "@~D=~A" pos string)
        string)))

(defun content-range (list start end)
  (typecase list
    (list (let ((len (length list)))
            (subseq list start (min len end))))
    (lcons (llist-range list start end))))

(defslyfun inspector-nth-part (index)
  "Return the current inspector's INDEXth part.
The second value indicates if that part exists at all."
  (let* ((parts (istate.parts (current-istate)))
         (foundp (< index (length parts))))
    (values (and foundp (aref parts index))
            foundp)))

(defslyfun inspector-nth-part-or-lose (index)
  "Return the current inspector's INDEXth part.
The second value indicates if that part exists at all."
  (multiple-value-bind (part foundp)
      (inspector-nth-part index)
    (if foundp part (error "No part with index ~a" index))))

(defslyfun inspect-nth-part (index)
  (with-buffer-syntax ()
    (inspect-object (inspector-nth-part index))))

(defslyfun inspector-range (from to)
  (prepare-range (current-istate) from to))

(defslyfun inspector-call-nth-action (index &rest args)
  (destructuring-bind (fun refreshp) (aref (istate.actions (current-istate)) index)
    (apply fun args)
    (if refreshp
        (inspector-reinspect)
        ;; tell emacs that we don't want to refresh the inspector buffer
        nil)))

(defslyfun inspector-pop ()
  "Inspect the previous object.
Return nil if there's no previous object."
  (with-buffer-syntax ()
    (let* ((history (inspector-%history (current-inspector))))
      (when (> (length history) 1)
        (decf (fill-pointer history))
        (aref history (fill-pointer history))
        (istate>elisp (current-istate))))))

(defslyfun inspector-next ()
  "Inspect the next element in the history of inspected objects.."
  (with-buffer-syntax ()
    (let* ((history (inspector-%history (current-inspector))))
      (when (and (< (fill-pointer history)
                    (array-dimension history 0))
                 (istate-p (aref history (fill-pointer history))))
        (incf (fill-pointer history))
        (istate>elisp (current-istate))))))

(defslyfun inspector-reinspect ()
  (let ((istate (current-istate)))
    (setf (istate.content istate)
          (emacs-inspect (istate.object istate)))
    (istate>elisp istate)))

(defslyfun inspector-toggle-verbose ()
  "Toggle verbosity of inspected object."
  (setf (inspector-verbose-p (current-inspector))
        (not (inspector-verbose-p (current-inspector))))
  (istate>elisp (current-istate)))

(defslyfun inspector-eval (string)
  (let* ((obj (istate.object (current-istate)))
         (context (eval-context obj))
         (form (with-buffer-syntax ((cdr (assoc '*package* context)))
                 (read-from-string string)))
         (ignorable (remove-if #'boundp (mapcar #'car context))))
    (to-string (eval `(let ((* ',obj) (- ',form)
                            . ,(loop for (var . val) in context
                                     unless (constantp var) collect
                                     `(,var ',val)))
                        (declare (ignorable . ,ignorable))
                        ,form)))))

(defslyfun inspector-history ()
  (slynk-pprint-to-line (inspector-%history (current-inspector))))

(defslyfun quit-inspector ()
  (reset-inspector)
  nil)

(defslyfun describe-inspectee ()
  "Describe the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string (istate.object (current-istate)))))

(defslyfun describe-inspector-part (index)
  "Describe part INDEX of the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string (inspector-nth-part index))))

(defslyfun pprint-inspector-part (index)
  "Pretty-print part INDEX of the currently inspected object."
  (with-buffer-syntax ()
    (slynk-pprint (inspector-nth-part index))))

(defslyfun inspect-in-frame (string index)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY inspection request.")
      (reset-inspector)
      (inspect-object (eval-in-frame (from-string string) index)))))

(defslyfun inspect-current-condition ()
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object *slynk-debugger-condition*)))

(defslyfun inspect-frame-var (frame var)
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object (frame-var-value frame var))))

(defslyfun pprint-frame-var (frame var)
  (with-buffer-syntax ()
    (slynk-pprint (frame-var-value frame var))))

(defslyfun describe-frame-var (frame var)
  (with-buffer-syntax ()
    (describe-to-string (frame-var-value frame var))))

(defslyfun eval-for-inspector (current
                               target
                               slave-slyfun &rest args)
  "Call SLAVE-SLYFUN with ARGS in CURRENT inspector, open in TARGET."
  (let ((*current-inspector* (and current
                                  (or (find-inspector current)
                                      (make-instance 'inspector :name current))))
        (*target-inspector* (and target
                                 (or (find-inspector target)
                                     (make-instance 'inspector :name target)))))
    (apply slave-slyfun args)))

;;;;; Lists

(defmethod emacs-inspect ((o cons))
  (if (listp (cdr o))
      (inspect-list o)
      (inspect-cons o)))

(defun inspect-cons (cons)
  (label-value-line*
   ('car (car cons))
   ('cdr (cdr cons))))

(defun inspect-list (list)
  (multiple-value-bind (length tail) (safe-length list)
    (flet ((frob (title list)
             (list* title '(:newline) (inspect-list-aux list))))
      (cond ((not length)
             (frob "A circular list:"
                   (cons (car list)
                         (ldiff (cdr list) list))))
            ((not tail)
             (frob "A proper list:" list))
            (t
             (frob "An improper list:" list))))))

(defun inspect-list-aux (list)
  (loop for i from 0  for rest on list  while (consp rest)  append
        (if (listp (cdr rest))
            (label-value-line i (car rest))
            (label-value-line* (i (car rest)) (:tail (cdr rest))))))

(defun safe-length (list)
  "Similar to `list-length', but avoid errors on improper lists.
Return two values: the length of the list and the last cdr.
Return NIL if LIST is circular."
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast list (cddr fast))          ;Fast pointer: leaps by 2.
       (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
      (nil)
    (cond ((null fast) (return (values n nil)))
          ((not (consp fast)) (return (values n fast)))
          ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
          ((and (eq fast slow) (> n 0)) (return nil))
          ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

;;;;; Hashtables

(defun hash-table-to-alist (ht)
  (let ((result '()))
    (maphash (lambda (key value)
               (setq result (acons key value result)))
             ht)
    result))

(defmethod emacs-inspect ((ht hash-table))
  (append
   (label-value-line*
    ("Count" (hash-table-count ht))
    ("Size" (hash-table-size ht))
    ("Test" (hash-table-test ht))
    ("Rehash size" (hash-table-rehash-size ht))
    ("Rehash threshold" (hash-table-rehash-threshold ht)))
   (let ((weakness (hash-table-weakness ht)))
     (when weakness
       (label-value-line "Weakness:" weakness)))
   (unless (zerop (hash-table-count ht))
     `((:action "[clear hashtable]"
                ,(lambda () (clrhash ht))) (:newline)
       "Contents: " (:newline)))
   (let ((content (hash-table-to-alist ht)))
     (cond ((every (lambda (x) (typep (first x) '(or string symbol))) content)
            (setf content (sort content 'string< :key #'first)))
           ((every (lambda (x) (typep (first x) 'number)) content)
            (setf content (sort content '< :key #'first))))
     (loop for (key . value) in content appending
           `((:value ,key) " = " (:value ,value)
             " " (:action "[remove entry]"
                          ,(let ((key key))
                                (lambda () (remhash key ht))))
             (:newline))))))

;;;;; Arrays

(defmethod emacs-inspect ((array array))
  (lcons*
   (iline "Dimensions" (array-dimensions array))
   (iline "Element type" (array-element-type array))
   (iline "Total size" (array-total-size array))
   (iline "Adjustable" (adjustable-array-p array))
   (iline "Fill pointer" (if (array-has-fill-pointer-p array)
                             (fill-pointer array)))
   "Contents:" '(:newline)
   (labels ((k (i max)
              (cond ((= i max) '())
                    (t (lcons (iline i (row-major-aref array i))
                              (k (1+ i) max))))))
     (k 0 (array-total-size array)))))

;;;;; Chars

(defmethod emacs-inspect :around (object)
  (declare (ignore object))
  (with-bindings (if (inspector-verbose-p (current-inspector))
                     *inspector-verbose-printer-bindings*
                     *inspector-printer-bindings*)
    (call-next-method)))

(defmethod emacs-inspect ((char character))
  (append
   (label-value-line*
    ("Char code" (char-code char))
    ("Lower cased" (char-downcase char))
    ("Upper cased" (char-upcase char)))
   (if (get-macro-character char)
       `("In the current readtable ("
         (:value ,*readtable*) ") it is a macro character: "
         (:value ,(get-macro-character char))))))

;;;; Thread listing

(defvar *thread-list* ()
  "List of threads displayed in Emacs.  We don't care a about
synchronization issues (yet).  There can only be one thread listing at
a time.")

(defslyfun list-threads ()
  "Return a list (LABELS (ID NAME STATUS ATTRS ...) ...).
LABELS is a list of attribute names and the remaining lists are the
corresponding attribute values per thread.
Example:
  ((:id :name :status :priority)
   (6 \"slynk-indentation-cache-thread\" \"Semaphore timed wait\" 0)
   (5 \"reader-thread\" \"Active\" 0)
   (4 \"control-thread\" \"Semaphore timed wait\" 0)
   (2 \"Slynk Sentinel\" \"Semaphore timed wait\" 0)
   (1 \"listener\" \"Active\" 0)
   (0 \"Initial\" \"Sleep\" 0))"
  (setq *thread-list* (all-threads))
  (when (and *emacs-connection*
             (use-threads-p)
             ;; FIXME: hardcoded thread name
             (equalp (thread-name (current-thread)) "slynk-worker")) 
    (setf *thread-list* (delete (current-thread) *thread-list*)))
  (let* ((plist (thread-attributes (car *thread-list*)))
         (labels (loop for (key) on plist by #'cddr
                       collect key)))
    `((:id :name :status ,@labels)
      ,@(loop for thread in *thread-list*
              for name = (thread-name thread)
              for attributes = (thread-attributes thread)
              collect (list* (thread-id thread)
                             (string name)
                             (thread-status thread)
                             (loop for label in labels
                                   collect (getf attributes label)))))))

(defslyfun quit-thread-browser ()
  (setq *thread-list* nil))

(defun nth-thread (index)
  (nth index *thread-list*))

(defslyfun debug-nth-thread (index)
  (let ((connection *emacs-connection*))
    (interrupt-thread (nth-thread index)
                      (lambda ()
                        (invoke-or-queue-interrupt
                         (lambda ()
                           (with-connection (connection)
                             (simple-break))))))))

(defslyfun kill-nth-thread (index)
  (kill-thread (nth-thread index)))

(defslyfun start-slynk-server-in-thread (index port-file-name)
  "Interrupt the INDEXth thread and make it start a slynk server.
The server port is written to PORT-FILE-NAME."
  (interrupt-thread (nth-thread index)
                    (lambda ()
                      (start-server port-file-name :style nil))))

;;;; Class browser

(defun mop-helper (class-name fn)
  (let ((class (find-class class-name nil)))
    (if class
        (mapcar (lambda (x) (to-string (class-name x)))
                (funcall fn class)))))

(defslyfun mop (type symbol-name)
  "Return info about classes using mop.

    When type is:
     :subclasses - return the list of subclasses of class.
     :superclasses - return the list of superclasses of class."
  (let ((symbol (parse-symbol symbol-name *buffer-package*)))
    (ecase type
      (:subclasses
       (mop-helper symbol #'ls-mop:class-direct-subclasses))
      (:superclasses
       (mop-helper symbol #'ls-mop:class-direct-superclasses)))))


;;;; Automatically synchronized state
;;;
;;; Here we add hooks to push updates of relevant information to
;;; Emacs.

;;;;; *FEATURES*

(defun sync-features-to-emacs ()
  "Update Emacs if any relevant Lisp state has changed."
  ;; FIXME: *sly-features* should be connection-local
  (unless (eq *sly-features* *features*)
    (setq *sly-features* *features*)
    (send-to-emacs (list :new-features (features-for-emacs)))))

(defun features-for-emacs ()
  "Return `*sly-features*' in a format suitable to send it to Emacs."
  *sly-features*)

(add-hook *pre-reply-hook* 'sync-features-to-emacs)


;;;;; Indentation of macros
;;;
;;; This code decides how macros should be indented (based on their
;;; arglists) and tells Emacs. A per-connection cache is used to avoid
;;; sending redundant information to Emacs -- we just say what's
;;; changed since last time.
;;;
;;; The strategy is to scan all symbols, pick out the macros, and look
;;; for &body-arguments.

(defvar *configure-emacs-indentation* t
  "When true, automatically send indentation information to Emacs
after each command.")

(defslyfun update-indentation-information ()
  (send-to-indentation-cache `(:update-indentation-information))
  nil)

;; This function is for *PRE-REPLY-HOOK*.
(defun sync-indentation-to-emacs ()
  "Send any indentation updates to Emacs via CONNECTION."
  (when *configure-emacs-indentation*
    (send-to-indentation-cache `(:sync-indentation ,*buffer-package*))))

;; Send REQUEST to the cache.  If we are single threaded perform the
;; request right away, otherwise delegate the request to the
;; indentation-cache-thread.
(defun send-to-indentation-cache (request)
  (let ((c *emacs-connection*))
    (etypecase c
      (singlethreaded-connection
       (handle-indentation-cache-request c request))
      (multithreaded-connection
       (without-sly-interrupts
         (send (mconn.indentation-cache-thread c) request))))))

(defun indentation-cache-loop (connection)
  (with-connection (connection)
    (loop
      (restart-case
          (handle-indentation-cache-request connection (receive))
        (abort ()
          :report "Return to the indentation cache request handling loop.")))))

(defun handle-indentation-cache-request (connection request)
  (destructure-case request
    ((:sync-indentation package)
     ;; PACKAGE may have been deleted...
     (when (package-name package)
       (let ((fullp (need-full-indentation-update-p connection)))
         (perform-indentation-update connection fullp package))))
    ((:update-indentation-information)
     (perform-indentation-update connection t nil))))

(defun need-full-indentation-update-p (connection)
  "Return true if the whole indentation cache should be updated.
This is a heuristic to avoid scanning all symbols all the time:
instead, we only do a full scan if the set of packages has changed."
  (set-difference (list-all-packages)
                  (connection-indentation-cache-packages connection)))

(defun perform-indentation-update (connection force package)
  "Update the indentation cache in CONNECTION and update Emacs.
If FORCE is true then start again without considering the old cache."
  (let ((cache (connection-indentation-cache connection)))
    (when force (clrhash cache))
    (let ((delta (update-indentation/delta-for-emacs cache force package)))
      (setf (connection-indentation-cache-packages connection)
            (list-all-packages))
      (unless (null delta)
        (setf (connection-indentation-cache connection) cache)
        (send-to-emacs (list :indentation-update delta))))))

(defun update-indentation/delta-for-emacs (cache force package)
  "Update the cache and return the changes in a (SYMBOL INDENT PACKAGES) list.
If FORCE is true then check all symbols, otherwise only check symbols
belonging to PACKAGE."
  (let ((alist '()))
    (flet ((consider (symbol)
             (let ((indent (symbol-indentation symbol)))
               (when indent
                 (unless (equal (gethash symbol cache) indent)
                   (setf (gethash symbol cache) indent)
                   (let ((pkgs (mapcar #'package-name
                                       (symbol-packages symbol)))
                         (name (string-downcase symbol)))
                     (push (list name indent pkgs) alist)))))))
      (cond (force
             (do-all-symbols (symbol)
               (consider symbol)))
            ((package-name package) ; don't try to iterate over a
                                    ; deleted package.
             (do-symbols (symbol package)
               (when (eq (symbol-package symbol) package)
                 (consider symbol)))))
      alist)))

(defun package-names (package)
  "Return the name and all nicknames of PACKAGE in a fresh list."
  (cons (package-name package) (copy-list (package-nicknames package))))

(defun symbol-packages (symbol)
  "Return the  packages where SYMBOL can be found."
  (let ((string (string symbol)))
    (loop for p in (list-all-packages)
          when (eq symbol (find-symbol string p))
          collect p)))

(defun cl-symbol-p (symbol)
  "Is SYMBOL a symbol in the COMMON-LISP package?"
  (eq (symbol-package symbol) cl-package))

(defun known-to-emacs-p (symbol)
  "Return true if Emacs has special rules for indenting SYMBOL."
  (cl-symbol-p symbol))

(defun symbol-indentation (symbol)
  "Return a form describing the indentation of SYMBOL.
The form is to be used as the `common-lisp-indent-function' property
in Emacs."
  (if (and (macro-function symbol)
           (not (known-to-emacs-p symbol)))
      (let ((arglist (arglist symbol)))
        (etypecase arglist
          ((member :not-available)
           nil)
          (list
           (macro-indentation arglist))))
      nil))

(defun macro-indentation (arglist)
  (if (well-formed-list-p arglist)
      (position '&body (remove '&optional (clean-arglist arglist)))
      nil))

(defun clean-arglist (arglist)
  "Remove &whole, &enviroment, and &aux elements from ARGLIST."
  (cond ((null arglist) '())
        ((member (car arglist) '(&whole &environment))
         (clean-arglist (cddr arglist)))
        ((eq (car arglist) '&aux)
         '())
        (t (cons (car arglist) (clean-arglist (cdr arglist))))))

(defun well-formed-list-p (list)
  "Is LIST a proper list terminated by NIL?"
  (typecase list
    (null t)
    (cons (well-formed-list-p (cdr list)))
    (t    nil)))

(defun print-indentation-lossage (&optional (stream *standard-output*))
  "Return the list of symbols whose indentation styles collide incompatibly.
Collisions are caused because package information is ignored."
  (let ((table (make-hash-table :test 'equal)))
    (flet ((name (s) (string-downcase (symbol-name s))))
      (do-all-symbols (s)
        (setf (gethash (name s) table)
              (cons s (symbol-indentation s))))
      (let ((collisions '()))
        (do-all-symbols (s)
          (let* ((entry (gethash (name s) table))
                 (owner (car entry))
                 (indent (cdr entry)))
            (unless (or (eq s owner)
                        (equal (symbol-indentation s) indent)
                        (and (not (fboundp s))
                             (null (macro-function s))))
              (pushnew owner collisions)
              (pushnew s collisions))))
        (if (null collisions)
            (format stream "~&No worries!~%")
            (format stream "~&Symbols with collisions:~%~{  ~S~%~}"
                    collisions))))))

;;; FIXME: it's too slow on CLASP right now, remove once it's fast enough.
#-clasp
(add-hook *pre-reply-hook* 'sync-indentation-to-emacs)


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


;;;; The "official" API

(defpackage :ls-api (:use))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((api '(#:*emacs-connection*
               #:default-connection
               ;;
               #:channel
               #:channel-id
               #:channel-thread-id
               #:close-channel
               #:define-channel-method
               #:find-channel
               #:send-to-remote-channel
               ;;
               #:listener
               #:with-listener-bindings
               #:saving-listener-bindings
               #:flush-listener-streams
               #:default-listener
               #:close-listener
               ;;
               #:add-hook
               #:*connection-closed-hook*
               #:*after-init-hook*
               #:*new-connection-hook*
               #:*pre-reply-hook*
               #:*after-toggle-trace-hook*
               #:*eval-for-emacs-wrappers*
               #:*debugger-extra-options*
               #:*buffer-readtable*
               ;;
               #:defslyfun
               #:destructure-case
               #:log-event
               #:process-requests
               #:use-threads-p
               #:wait-for-event
               #:with-bindings
               #:with-connection
               #:with-top-level-restart
               #:with-sly-interrupts
               #:with-buffer-syntax
               #:with-retry-restart
               #:load-user-init-file
               #:make-thread-bindings-aware-lambda
               ;;
               #:package-string-for-prompt
               ;;
               #:*slynk-wire-protocol-version*
               ;;
               #:*slynk-require-hook*
               ;;
               #:present-for-emacs
               ;; packages
               ;;
               #:cl-package
               #:keyword-package
               #:guess-package
               #:guess-buffer-package
               ;; symbols
               ;;
               #:tokenize-symbol
               #:untokenize-symbol
               #:symbol-external-p
               #:unparse-name
               #:excluded-from-searches-p
               ;;
               ;;
               #:slynk-pprint
               #:slynk-pprint-values
               #:slynk-pprint-to-line)))
    (loop for sym in api
          for slynk-api-sym = (intern (string sym) :ls-api)
          for slynk-sym = (intern (string sym) :ls-base)
          do (unintern slynk-api-sym :ls-api)
             (import slynk-sym :ls-api)
             (export slynk-sym :ls-api))))


;;;; INIT, as called from the slynk-loader.lisp and ASDF's loaders
;;;; 
(defun load-user-init-file ()
  "Load the user init file, return NIL if it does not exist."
  (some (lambda (homedir-file)
          (load (merge-pathnames (user-homedir-pathname)
                                 homedir-file)
                :if-does-not-exist nil))
        (list (make-pathname :name ".slynk" :type "lisp")
              (make-pathname :name ".slynkrc")
              (make-pathname :name ".swank" :type "lisp")
              (make-pathname :name ".swankrc"))))

(defun init ()
  (unless (member :language-server *features*)
    (pushnew :language-server *features*))
  (load-user-init-file)
  (run-hook *after-init-hook*))

;; Local Variables:
;; sly-load-failed-fasl: ask
;; End:
