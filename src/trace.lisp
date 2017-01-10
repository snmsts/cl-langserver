(in-package :ls-base)
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
