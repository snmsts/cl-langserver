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
