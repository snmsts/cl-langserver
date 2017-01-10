(in-package :ls-base)
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
