(in-package :ls-base)
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
