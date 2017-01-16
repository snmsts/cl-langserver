;; thinking about generalized protocol for repl. on jsonrpc/websocket?.

;; -> call from client
;; <- call from server
;; |> notify from client
;; <| notify from server
;;                to server          from server
;; -> create-repl()                (channel-id,env)        ;;env could have prompt/banner etc and implementation extensible.
;; <| env-update                   (channel-id,env)
;; -> eval(channel-id,exp,env,opt) (result)                ;; need to think about generalized opt.
;; <- dialog                       (channel-id,msg,choice) ;; y-n/debugger/
;; <| writeString                  (channel-id,str)


;;impl
;;*slynk-debug-p* -> *langserver-debug-p*
;;signal-slynk-error -> signal-langserver-error
;;defslyfun -> defrpcfun
;;with-slynk-error-handler -> with-langserver-error-handler
;;*emacs-connection* -> *client-connection*
;;*slynk-io-package* -> *langserver-io-package*
;;send-to-emacs -> send-to-client
;;*sly-interrupts-enabled* -> *client-interrupts-enabled*
;;without-sly-interrupts -> without-client-interrupts
;;slynk-debugger-hook -> langserver-debugger-hook

;;note
;; don't support (clisp cmucl corman ecl) cause not spawn
