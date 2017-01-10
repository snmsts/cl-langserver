(in-package :ls-base)
;;;; Compilation Commands.

(defstruct (compilation-result
            (:type list) :named)
  notes
  (successp nil :type boolean)
  (duration 0.0 :type float)
  (loadp nil :type boolean)
  (faslfile nil :type (or null string)))

(defun measure-time-interval (fun)
  "Call FUN and return the first return value and the elapsed time.
The time is measured in seconds."
  (declare (type function fun))
  (let ((before (get-internal-real-time))) ;
    (values
     (funcall fun)
     (/ (- (get-internal-real-time) before)
        (coerce internal-time-units-per-second 'float)))))

(defun make-compiler-note (condition)
  "Make a compiler note data structure from a compiler-condition."
  (declare (type compiler-condition condition))
  (list* :message (message condition)
         :severity (severity condition)
         :location (location condition)
         :references (references condition)
         (let ((s (source-context condition)))
           (if s (list :source-context s)))))

(defun collect-notes (function)
  (let ((notes '()))
    (multiple-value-bind (result seconds)
        (handler-bind ((compiler-condition
                        (lambda (c) (push (make-compiler-note c) notes))))
          (measure-time-interval
           (lambda ()
             ;; To report location of error-signaling toplevel forms
             ;; for errors in EVAL-WHEN or during macroexpansion.
             (restart-case (multiple-value-list (funcall function))
               (abort () :report "Abort compilation." (list nil))))))
      (destructuring-bind (successp &optional loadp faslfile) result
        (let ((faslfile (etypecase faslfile
                          (null nil)
                          (pathname (pathname-to-filename faslfile)))))
          (make-compilation-result :notes (reverse notes)
                                   :duration seconds
                                   :successp (if successp t)
                                   :loadp (if loadp t)
                                   :faslfile faslfile))))))

(defun slynk-compile-file* (pathname load-p &rest options &key policy
                                                      &allow-other-keys)
  (multiple-value-bind (output-pathname warnings? failure?)
      (slynk-compile-file pathname
                          (fasl-pathname pathname options)
                          nil
                          (or (guess-external-format pathname)
                              :default)
                          :policy policy)
    (declare (ignore warnings?))
    (values t (not failure?) load-p output-pathname)))

(defvar *compile-file-for-emacs-hook* '(slynk-compile-file*))

(defslyfun compile-file-for-emacs (filename load-p &rest options)
  "Compile FILENAME and, when LOAD-P, load the result.
Record compiler notes signalled as `compiler-condition's."
  (with-buffer-syntax ()
    (collect-notes
     (lambda ()
       (let ((pathname (filename-to-pathname filename))
             (*compile-print* nil)
             (*compile-verbose* t))
         (loop for hook in *compile-file-for-emacs-hook*
               do
               (multiple-value-bind (tried success load? output-pathname)
                   (apply hook pathname load-p options)
                 (when tried
                   (return (values success load? output-pathname))))))))))

;; FIXME: now that *compile-file-for-emacs-hook* is there this is
;; redundant and confusing.
(defvar *fasl-pathname-function* nil
  "In non-nil, use this function to compute the name for fasl-files.")

(defun pathname-as-directory (pathname)
  (append (pathname-directory pathname)
          (when (pathname-name pathname)
            (list (file-namestring pathname)))))

(defun compile-file-output (file directory)
  (make-pathname :directory (pathname-as-directory directory)
                 :defaults (compile-file-pathname file)))

(defun fasl-pathname (input-file options)
  (cond (*fasl-pathname-function*
         (funcall *fasl-pathname-function* input-file options))
        ((getf options :fasl-directory)
         (let ((dir (getf options :fasl-directory)))
           (assert (char= (aref dir (1- (length dir))) #\/))
           (compile-file-output input-file dir)))
        (t
         (compile-file-pathname input-file))))

(defslyfun compile-string-for-emacs (string buffer position filename policy)
  "Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (let ((offset (cadr (assoc :position position))))
    (with-buffer-syntax ()
      (collect-notes
       (lambda ()
         (let ((*compile-print* nil)
               (*compile-verbose* nil)
               (*load-verbose* nil))
           (slynk-compile-string string
                                 :buffer buffer
                                 :position offset
                                 :filename filename
                                 :policy policy)))))))

(defslyfun compile-multiple-strings-for-emacs (strings policy)
  "Compile STRINGS (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (loop for (string buffer package position filename) in strings collect
        (collect-notes
         (lambda ()
           (with-buffer-syntax (package)
             (let ((*compile-print* t) (*compile-verbose* nil))
               (slynk-compile-string string
                                     :buffer buffer
                                     :position position
                                     :filename filename
                                     :policy policy)))))))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun requires-compile-p (source-file)
  (let ((fasl-file (probe-file (compile-file-pathname source-file))))
    (or (not fasl-file)
        (file-newer-p source-file fasl-file))))

(defslyfun compile-file-if-needed (filename loadp)
  (let ((pathname (filename-to-pathname filename)))
    (cond ((requires-compile-p pathname)
           (compile-file-for-emacs pathname loadp))
          (t
           (collect-notes
            (lambda ()
              (or (not loadp)
                  (load (compile-file-pathname pathname)))))))))


;;;; Loading

(defslyfun load-file (filename)
  (to-string (load (filename-to-pathname filename))))


;;;;; slynk-require

(defvar *module-loading-method* (find-if #'find-package '(:ls-loader :asdf))
  "Keyword naming the module-loading method.

SLY's own `slynk-loader.lisp' is tried first, then ASDF")

(defgeneric require-module (method module)
  (:documentation
   "Use METHOD to load MODULE.
Receives a module name as argument and should return non-nil if it
managed to load it.")
  (:method ((method (eql :ls-loader)) module)
    (funcall (intern "REQUIRE-MODULE" :ls-loader) module))
  (:method ((method (eql :asdf)) module)
    (funcall (intern "LOAD-SYSTEM" :asdf) module)))

(defun add-to-load-path-1 (path load-path-var)
  (pushnew path (symbol-value load-path-var) :test #'equal))

(defgeneric add-to-load-path (method path)
  (:documentation
   "Using METHOD, consider PATH when searching for modules.")
  (:method ((method (eql :ls-loader)) path)
    (add-to-load-path-1 path (intern "*LOAD-PATH*" :ls-loader)))
  (:method ((method (eql :asdf)) path)
    (add-to-load-path-1 path (intern "*CENTRAL-REGISTRY*" :asdf))))

(defvar *slynk-require-hook* '()
  "Functions run after SLYNK-REQUIRE. Called with new modules.")

(defslyfun slynk-require (modules)
  "Load each module in MODULES.

MODULES is a list of strings designators or a single string
designator. Returns a list of all modules available."
  (let ((loaded))
    (dolist (module (ensure-list modules))
      (with-simple-restart (continue "Continue without SLY contrib ~a" module)
        (funcall #'require-module *module-loading-method* module)
        (push module loaded)
        (pushnew (string-upcase module) *modules* :test #'equal))
      (loop for fn in *slynk-require-hook*
            do (funcall fn loaded)))
    (list *modules* loaded)))

(defslyfun slynk-add-load-paths (paths)
  (dolist (path paths)
    (funcall #'add-to-load-path *module-loading-method* (pathname path))))
