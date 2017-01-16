(in-package :langserver-base)

;;;; TCP Server

(defvar *communication-style* (preferred-communication-style))

(defvar *dont-close* nil
  "Default value of :dont-close argument to start-server and
  create-server.")

(defun start-server (port-file &key (style *communication-style*)
                                    (dont-close *dont-close*))
  "Start the server and write the listen port number to PORT-FILE.
This is the entry point for Emacs."
  (setup-server 0
                (lambda (port) (announce-server-port port-file port))
                style dont-close nil))

(defun create-server (&key (port default-server-port)
                        (style *communication-style*)
                        (dont-close *dont-close*)
                        backlog)
  "Start a langserver server on PORT running in STYLE.
If DONT-CLOSE is true then the listen socket will accept multiple
connections, otherwise it will be closed after the first."
  (setup-server port #'simple-announce-function
                style dont-close backlog))

(defun find-external-format-or-lose (coding-system)
  (or (find-external-format coding-system)
      (error "Unsupported coding system: ~s" coding-system)))

(defparameter *loopback-interface* "127.0.0.1")

(defmacro restart-loop (form &body clauses)
  "Executes FORM, with restart-case CLAUSES which have a chance to modify FORM's
environment before trying again (by returning normally) or giving up (through an
explicit transfer of control), all within an implicit block named nil.
e.g.: (restart-loop (http-request url) (use-value (new) (setq url new)))"
  `(loop (restart-case (return ,form) ,@clauses)))

(defun socket-quest (port backlog)
  "Attempt o create a socket on PORT.
Add a restart, prompting user to enter a new port if PORT is already
taken."
  (restart-loop (create-socket *loopback-interface* port :backlog backlog)
    (use-value (&optional (new-port (1+ port)))
      :report (lambda (stream) (format stream "Try a port other than ~D" port))
      :interactive
      (lambda ()
        (format *query-io* "Enter port (defaults to ~D): " (1+ port))
        (finish-output *query-io*)      ; necessary for tunnels
        (ignore-errors (list (parse-integer (read-line *query-io*)))))
      (setq port new-port))))

(defun setup-server (port announce-fn style dont-close backlog)
  (init-log-output)
  (let* ((socket (socket-quest port backlog))
         (port (local-port socket)))
    (funcall announce-fn port)
    (labels ((serve () (accept-connections socket style dont-close))
             (note () (send-to-sentinel `(:add-server ,socket ,port
                                                      ,(current-thread))))
             (serve-loop () (note) (loop do (serve) while dont-close)))
      (ecase style
        (:spawn (initialize-multiprocessing
                 (lambda ()
                   (start-sentinel)
                   (spawn #'serve-loop :name (format nil "langserver ~s" port)))))
        ((:fd-handler :sigio)
         (note)
         (add-fd-handler socket #'serve))
        ((nil) (serve-loop))))
    port))

(defun stop-server (port)
  "Stop server running on PORT."
  (send-to-sentinel `(:stop-server :port ,port)))

(defun restart-server (&key (port default-server-port)
                       (style *communication-style*)
                       (dont-close *dont-close*))
  "Stop the server listening on PORT, then start a new Langserver server
on PORT running in STYLE. If DONT-CLOSE is true then the listen socket
will accept multiple connections, otherwise it will be closed after the
first."
  (stop-server port)
  (sleep 5)
  (create-server :port port :style style :dont-close dont-close))

(defun accept-connections (socket style dont-close)
  (let ((client (unwind-protect
                     (accept-connection socket :external-format nil
                                               :buffering t)
                  (unless dont-close
                    (close-socket socket)))))
    (serve-requests (make-connection socket client style))
    (unless dont-close
      (send-to-sentinel `(:stop-server :socket ,socket)))))

(defun serve-requests (connection)
  "Read and process all requests on connections."
  (etypecase connection
    (multithreaded-connection
     (spawn-threads-for-connection connection))
    (singlethreaded-connection
     (ecase (connection-communication-style connection)
       ((nil) (simple-serve-requests connection))
       (:sigio (install-sigio-handler connection))
       (:fd-handler (install-fd-handler connection))))))

(defun stop-serving-requests (connection)
  (etypecase connection
    (multithreaded-connection
     (cleanup-connection-threads connection))
    (singlethreaded-connection
     (ecase (connection-communication-style connection)
       ((nil))
       (:sigio (deinstall-sigio-handler connection))
       (:fd-handler (deinstall-fd-handler connection))))))

(defun announce-server-port (file port)
  (with-open-file (s file
                     :direction :output
                     :if-exists :error
                     :if-does-not-exist :create)
    (format s "~S~%" port))
  (simple-announce-function port))

(defun simple-announce-function (port)
  (when *langserver-debug-p*
    (format *log-output* "~&;; langserver started at port: ~D.~%" port)
    (force-output *log-output*)))


;;;;; Event Decoding/Encoding

(defun decode-message (stream)
  "Read an S-expression from STREAM using the langserver protocol."
  (log-event "decode-message~%")
  (without-client-interrupts
    (handler-bind ((error #'signal-langserver-error))
      (handler-case (read-message stream *langserver-io-package*)
        #|(slynk-reader-error (c)
        `(:reader-error ,(slynk-reader-error.packet c)
        ,(slynk-reader-error.cause c)))|#
        ))))

(defun encode-message (message stream)
  "Write an S-expression to STREAM using the langserver protocol."
  (log-event "encode-message~%")
  (without-client-interrupts
    (handler-bind ((error #'signal-langserver-error))
      (write-message message *langserver-io-package* stream))))
