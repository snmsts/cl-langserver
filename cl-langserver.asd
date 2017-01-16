;;; -*- lisp -*-
(in-package :asdf)

;; ASDF system definition for loading the Slynk server independently
;; of Emacs.
;;
;; Usage:
;;
;;   (push #p"/path/to/this/file/" asdf:*central-registry*)
;;   (asdf:load-system :langserver-helitage)
;;   (slynk:create-server :port PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Slynk server is running on localhost:ACTUAL-PORT. You can
;; use `M-x sly-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.
(defsystem :langserver-backend
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
       (:file "slynk-gray")))))))

(defsystem :cl-langserver
  :depends-on
  (#+ros.init "fukamachi/jsonrpc"
   #-ros.init :jsonrpc
   :langserver-backend)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "defs")
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
     #|
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
     (:file "arglist")
     (:file "documentation")
     (:file "package")
     (:file "trace")
     (:file "undefine")
     (:file "source-location")
     (:file "lazy-list")
     (:file "inspect")
     (:file "thread-list")
     (:file "class-browser")
     (:file "sync")
     (:file "indentation")
     (:file "test")
     (:file "api")
     (:file "init")
     (:file "slynk-completion")
     |#))))

(defsystem :langserver-helitage
  :depends-on (:langserver-backend)
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "helitage"
      :serial t
      :components
      ((:file "slynk-match")
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
       (:file "arglist")
       (:file "documentation")
       (:file "package")
       (:file "trace")
       (:file "undefine")
       (:file "source-location")
       (:file "lazy-list")
       (:file "inspect")
       (:file "thread-list")
       (:file "class-browser")
       (:file "sync")
       (:file "indentation")
       (:file "test")
       (:file "api")
       (:file "init")
       (:file "slynk-completion")))))))

(defsystem :langserver-helitage-util
  :components ((:file "src/helitage/slynk-util")))

(defmethod perform :after ((o load-op) (c (eql (find-system :langserver-helitage))))
  (format *debug-io* "~&SLYNK's ASDF loader finished.")
  (funcall (read-from-string "ls-base::init")))

#+sbcl
(defmethod operate :around ((o load-op) (c (eql (find-system :langserver-helitage))) &key &allow-other-keys)
  (let ((asdf:*compile-file-failure-behaviour* :warn)
        (sb-ext:*on-package-variance* '(:warn t)))
    (call-next-method)))


;;; Contrib systems (should probably go into their own file one day)
;;;
(defsystem :langserver-helitage-arglists
  :depends-on (:langserver-helitage)
  :components ((:file "src/contrib/slynk-arglists")))

(defsystem :langserver-helitage-fancy-inspector
  :depends-on (:langserver-helitage :langserver-helitage-util)
  :components ((:file "src/contrib/slynk-fancy-inspector")))

(defsystem :langserver-helitage-package-fu
  :depends-on (:langserver-helitage)
  :components ((:file "src/contrib/slynk-package-fu")))

(defsystem :langserver-helitage-mrepl
  :depends-on (:langserver-helitage)
  :components ((:file "src/contrib/slynk-mrepl")))

(defsystem :langserver-helitage-trace-dialog
  :depends-on (:langserver-helitage)
  :components ((:file "src/contrib/slynk-trace-dialog")))

(defsystem :langserver-helitage-profiler
  :depends-on (:langserver-helitage)
  :components ((:file "src/contrib/slynk-profiler")))

(defsystem :langserver-helitage-stickers
  :depends-on (:langserver-helitage)
  :components ((:file "src/contrib/slynk-stickers")))

(defsystem :langserver-helitage-indentation
  :depends-on (:langserver-helitage)
  :components ((:file "src/contrib/slynk-indentation")))

(defsystem :langserver-helitage-retro
  :depends-on (:langserver-helitage)
  :components ((:file "src/contrib/slynk-retro")))
