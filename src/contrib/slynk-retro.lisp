(defpackage :ls-retro
  (:use :cl :ls-base :ls-api))

(in-package :ls-retro)

(defun ensure-slynk-package-nicknames (&rest ignored)
  "Nickname all SLYNK-* package to SWANK-*"
  (declare (ignore ignored))
  (loop for package in (list-all-packages)
      for package-name = (package-name package)
      when (search "SLYNK" package-name :test #'char-equal)
        do (rename-package package
                           package-name
                           (remove-duplicates
                            (cons
                             (format nil "SWANK~a"
                                     (subseq package-name 5))
                             (package-nicknames package))
                            :test #'string-equal))))

(setq ls-rpc:*translating-swank-to-slynk* nil)
(push #'ensure-slynk-package-nicknames
      ls-api:*slynk-require-hook*)

(ensure-slynk-package-nicknames)

(provide :ls-retro)


