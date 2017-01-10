(in-package :ls-base)
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
