(in-package :ls-base)
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
