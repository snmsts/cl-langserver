(in-package :ls-base)
;;;; The "official" API

(defpackage :ls-api (:use))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((api '(#:*emacs-connection*
               #:default-connection
               ;;
               #:channel
               #:channel-id
               #:channel-thread-id
               #:close-channel
               #:define-channel-method
               #:find-channel
               #:send-to-remote-channel
               ;;
               #:listener
               #:with-listener-bindings
               #:saving-listener-bindings
               #:flush-listener-streams
               #:default-listener
               #:close-listener
               ;;
               #:add-hook
               #:*connection-closed-hook*
               #:*after-init-hook*
               #:*new-connection-hook*
               #:*pre-reply-hook*
               #:*after-toggle-trace-hook*
               #:*eval-for-emacs-wrappers*
               #:*debugger-extra-options*
               #:*buffer-readtable*
               ;;
               #:defslyfun
               #:destructure-case
               #:log-event
               #:process-requests
               #:use-threads-p
               #:wait-for-event
               #:with-bindings
               #:with-connection
               #:with-top-level-restart
               #:with-sly-interrupts
               #:with-buffer-syntax
               #:with-retry-restart
               #:load-user-init-file
               #:make-thread-bindings-aware-lambda
               ;;
               #:package-string-for-prompt
               ;;
               #:*slynk-wire-protocol-version*
               ;;
               #:*slynk-require-hook*
               ;;
               #:present-for-emacs
               ;; packages
               ;;
               #:cl-package
               #:keyword-package
               #:guess-package
               #:guess-buffer-package
               ;; symbols
               ;;
               #:tokenize-symbol
               #:untokenize-symbol
               #:symbol-external-p
               #:unparse-name
               #:excluded-from-searches-p
               ;;
               ;;
               #:slynk-pprint
               #:slynk-pprint-values
               #:slynk-pprint-to-line)))
    (loop for sym in api
          for slynk-api-sym = (intern (string sym) :ls-api)
          for slynk-sym = (intern (string sym) :ls-base)
          do (unintern slynk-api-sym :ls-api)
             (import slynk-sym :ls-api)
             (export slynk-sym :ls-api))))
