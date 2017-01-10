(in-package :ls-base)
;;;; Simple arglist display

(defslyfun operator-arglist (name package)
  (ignore-errors
    (let ((args (arglist (parse-symbol name (guess-buffer-package package)))))
      (cond ((eq args :not-available) nil)
	    (t (princ-to-string (cons name args)))))))
