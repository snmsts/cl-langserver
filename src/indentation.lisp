(in-package :ls-base)
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
