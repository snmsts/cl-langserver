(in-package :ls-base)
;;;; Prompt

;; FIXME: do we really need 45 lines of code just to figure out the
;; prompt?

(defvar *canonical-package-nicknames*
  `((:common-lisp-user . :cl-user))
  "Canonical package names to use instead of shortest name/nickname.")
  
(defvar *auto-abbreviate-dotted-packages* t
  "Abbreviate dotted package names to their last component if T.")

(defun package-string-for-prompt (package)
  "Return the shortest nickname (or canonical name) of PACKAGE."
  (unparse-name
   (or (canonical-package-nickname package)
       (auto-abbreviated-package-name package)
       (shortest-package-nickname package))))

(defun canonical-package-nickname (package)
  "Return the canonical package nickname, if any, of PACKAGE."
  (let ((name (cdr (assoc (package-name package) *canonical-package-nicknames*
                          :test #'string=))))
    (and name (string name))))

(defun auto-abbreviated-package-name (package)
  "Return an abbreviated 'name' for PACKAGE.

N.B. this is not an actual package name or nickname."
  (when *auto-abbreviate-dotted-packages*
    (loop with package-name = (package-name package)
          with offset = nil
          do (let ((last-dot-pos (position #\. package-name :end offset
                                           :from-end t)))
               (unless last-dot-pos
                 (return nil))
               ;; If a dot chunk contains only numbers, that chunk most
               ;; likely represents a version number; so we collect the
               ;; next chunks, too, until we find one with meat.
               (let ((name (subseq package-name (1+ last-dot-pos) offset)))
                 (if (notevery #'digit-char-p name)
                     (return (subseq package-name (1+ last-dot-pos)))
                     (setq offset last-dot-pos)))))))

(defun shortest-package-nickname (package)
  "Return the shortest nickname of PACKAGE."
  (loop for name in (cons (package-name package) (package-nicknames package))
        for shortest = name then (if (< (length name) (length shortest))
                                   name
                                   shortest)
              finally (return shortest)))



(defslyfun ed-in-emacs (&optional what)
  "Edit WHAT in Emacs.

WHAT can be:
  A pathname or a string,
  A list (PATHNAME-OR-STRING &key LINE COLUMN POSITION),
  A function name (symbol or cons),
  NIL. "
  (flet ((canonicalize-filename (filename)
           (pathname-to-filename (or (probe-file filename) filename))))
    (let ((target
           (etypecase what
             (null nil)
             ((or string pathname)
              `(:filename ,(canonicalize-filename what)))
             ((cons (or string pathname) *)
              `(:filename ,(canonicalize-filename (car what)) ,@(cdr what)))
             ((or symbol cons)
              `(:function-name ,(prin1-to-string what))))))
      (cond (*emacs-connection* (send-oob-to-emacs `(:ed ,target)))
            ((default-connection)
             (with-connection ((default-connection))
               (send-oob-to-emacs `(:ed ,target))))
            (t (error "No connection"))))))

(defslyfun inspect-in-emacs (what &key wait)
  "Inspect WHAT in Emacs. If WAIT is true (default NIL) blocks until the
inspector has been closed in Emacs."
  (flet ((send-it ()
           (let ((tag (when wait (make-tag)))
                 (thread (when wait (current-thread-id))))
             (with-buffer-syntax ()
               (reset-inspector)
               (send-oob-to-emacs `(:inspect ,(inspect-object what)
                                             ,thread
                                             ,tag)))
             (when wait
               (wait-for-event `(:emacs-return ,tag result))))))
    (cond
      (*emacs-connection*
       (send-it))
      ((default-connection)
       (with-connection ((default-connection))
         (send-it))))
    what))

(defslyfun value-for-editing (form)
  "Return a readable value of FORM for editing in Emacs.
FORM is expected, but not required, to be SETF'able."
  ;; FIXME: Can we check FORM for setfability? -luke (12/Mar/2005)
  (with-buffer-syntax ()
    (let* ((value (eval (read-from-string form)))
           (*print-length* nil))
      (prin1-to-string value))))

(defslyfun commit-edited-value (form value)
  "Set the value of a setf'able FORM to VALUE.
FORM and VALUE are both strings from Emacs."
  (with-buffer-syntax ()
    (eval `(setf ,(read-from-string form)
            ,(read-from-string (concatenate 'string "`" value))))
    t))

(defun background-message  (format-string &rest args)
  "Display a message in Emacs' echo area.

Use this function for informative messages only.  The message may even
be dropped if we are too busy with other things."
  (when *emacs-connection*
    (send-to-emacs `(:background-message
                     ,(apply #'format nil format-string args)))))

;; This is only used by the test suite.
(defun sleep-for (seconds)
  "Sleep for at least SECONDS seconds.
This is just like cl:sleep but guarantees to sleep
at least SECONDS."
  (let* ((start (get-internal-real-time))
         (end (+ start
                 (* seconds internal-time-units-per-second))))
    (loop
     (let ((now (get-internal-real-time)))
       (cond ((< end now) (return))
             (t (sleep (/ (- end now)
                          internal-time-units-per-second))))))))
