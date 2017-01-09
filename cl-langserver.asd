;;; -*- lisp -*-
(in-package :asdf)

;; ASDF system definition for loading the Slynk server independently
;; of Emacs.
;;
;; Usage:
;;
;;   (push #p"/path/to/this/file/" asdf:*central-registry*)
;;   (asdf:load-system :slynk)
;;   (slynk:create-server :port PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Slynk server is running on localhost:ACTUAL-PORT. You can
;; use `M-x sly-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

(defsystem :cl-langserver
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "backend"
      :serial t
      :components
      ((:file "backend")
       ;; If/when we require ASDF3, we shall use :if-feature instead
       #+(or cmu sbcl scl)
       (:file "slynk-source-path-parser")
       #+(or cmu ecl sbcl scl)
       (:file "slynk-source-file-cache")
       #+clisp
       (:file "xref")
       #+(or clisp clozure)
       (:file "metering")
       (:module "impls"
        :serial t
        :components (#+allegro
                     (:file "allegro")
                     #+armedbear
                     (:file "abcl")
                     #+clisp
                     (:file "clisp")
                     #+clozure
                     (:file "ccl")
                     #+cmu
                     (:file "cmucl")
                     #+cormanlisp
                     (:file "corman")
                     #+ecl
                     (:file "ecl")
                     #+lispworks
                     (:file "lispworks")
                     #+sbcl
                     (:file "sbcl")
                     #+scl
                     (:file "scl")
                     #+mkcl
                     (:file "mkcl")))
       #-armedbear
       (:file "slynk-gray")))
     (:file "slynk-match")
     (:file "slynk-rpc")
     (:file "defs")
     (:file "hooks")
     (:file "connections")
     (:file "log")
     (:file "channels")
     (:file "listeners")
     (:file "interrupts")
     (:file "sentinel")
     (:file "misc")
     (:file "symbols")
     (:file "tcp")
     (:file "events")
     (:file "events-thread")
     (:file "events-flowcontrol")
     (:file "events-signal")
     (:file "events-serve-event")
     (:file "events-simple")
     (:file "inteructions")
     (:file "read-print")
     (:file "eval")
     (:file "prompt")
     (:file "debugger")
     (:file "compilation")
     (:file "macroexpansion")
     (:file "slynk")
     (:file "slynk-completion")))))

(defsystem :slynk-util
  :components ((:file "src/slynk-util")))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-langserver))))
  (format *debug-io* "~&SLYNK's ASDF loader finished.")
  (funcall (read-from-string "ls-base::init")))

#+sbcl
(defmethod operate :around ((o load-op) (c (eql (find-system :cl-langserver))) &key &allow-other-keys)
  (let ((asdf:*compile-file-failure-behaviour* :warn)
        (sb-ext:*on-package-variance* '(:warn t)))
    (call-next-method)))


;;; Contrib systems (should probably go into their own file one day)
;;;
(defsystem :slynk-arglists
  :depends-on (:cl-langserver)
  :components ((:file "src/contrib/slynk-arglists")))

(defsystem :slynk-fancy-inspector
  :depends-on (:cl-langserver :slynk-util)
  :components ((:file "src/contrib/slynk-fancy-inspector")))

(defsystem :slynk-package-fu
  :depends-on (:cl-langserver)
  :components ((:file "src/contrib/slynk-package-fu")))

(defsystem :slynk-mrepl
  :depends-on (:cl-langserver)
  :components ((:file "src/contrib/slynk-mrepl")))

(defsystem :slynk-trace-dialog
  :depends-on (:cl-langserver)
  :components ((:file "src/contrib/slynk-trace-dialog")))

(defsystem :slynk-profiler
  :depends-on (:cl-langserver)
  :components ((:file "src/contrib/slynk-profiler")))

(defsystem :slynk-stickers
  :depends-on (:cl-langserver)
  :components ((:file "src/contrib/slynk-stickers")))

(defsystem :slynk-indentation
  :depends-on (:cl-langserver)
  :components ((:file "src/contrib/slynk-indentation")))

(defsystem :slynk-retro
  :depends-on (:cl-langserver)
  :components ((:file "src/contrib/slynk-retro")))
