(in-package :ls-base)

;;;; Connections
;;;
;;; Connection structures represent the network connections between
;;; Emacs and Lisp. 
;;;
(defstruct (connection
             (:constructor %make-connection)
             (:conc-name connection-)
             (:print-function print-connection))
  ;; The listening socket. (usually closed)
  ;; 
  (socket           (missing-arg) :type t :read-only t)
  ;; Character I/O stream of socket connection.  Read-only to avoid
  ;; race conditions during initialization.
  ;; 
  (socket-io        (missing-arg) :type stream :read-only t)
  ;; An alist of (ID . CHANNEL) entries. Channels are good for
  ;; streaming data over the wire (see their description in sly.el)
  ;;
  (channel-counter 0 :type number)
  (channels '() :type list)
  ;; A list of LISTENER objects. Each listener has a couple of streams
  ;; and an environment (an alist of bindings)
  ;;
  (listeners '() :type list)
  ;; A list of INSPECTOR objects. Each inspector has its own history
  ;; of inspected objects. An inspector might also be tied to a
  ;; specific thread.
  ;; 
  (inspectors '() :type list)
  ;;Cache of macro-indentation information that
  ;; has been sent to Emacs.  This is used for preparing deltas to
  ;; update Emacs's knowledge.  Maps: symbol ->
  ;; indentation-specification
  ;; 
  (indentation-cache (make-hash-table :test 'eq) :type hash-table)
  ;; The list of packages represented in the cache:
  ;; 
  (indentation-cache-packages '())
  ;; The communication style used.
  ;; 
  (communication-style nil :type (member nil :spawn :sigio :fd-handler))
  )

(defun print-connection (conn stream depth)
  (declare (ignore depth))
  (print-unreadable-object (conn stream :type t :identity t)))

(defstruct (singlethreaded-connection (:include connection)
                                      (:conc-name sconn.))
  ;; The SIGINT handler we should restore when the connection is
  ;; closed.
  saved-sigint-handler
  ;; A queue of events.  Not all events can be processed in order and
  ;; we need a place to stored them.
  (event-queue '() :type list)
  ;; A counter that is incremented whenever an event is added to the
  ;; queue.  This is used to detected modifications to the event queue
  ;; by interrupts.  The counter wraps around.
  (events-enqueued 0 :type fixnum))

(defstruct (multithreaded-connection (:include connection)
                                     (:conc-name mconn.))
  ;; In multithreaded systems we delegate certain tasks to specific
  ;; threads. The `reader-thread' is responsible for reading network
  ;; requests from Emacs and sending them to the `control-thread'; the
  ;; `control-thread' is responsible for dispatching requests to the
  ;; threads that should handle them.
  reader-thread
  control-thread
  auto-flush-thread
  indentation-cache-thread
  ;; List of threads that are currently processing requests.  We use
  ;; this to find the newest/current thread for an interrupt.  In the
  ;; future we may store here (thread . request-tag) pairs so that we
  ;; can interrupt specific requests.
  (active-threads '() :type list)
  )

(defvar *emacs-connection* nil
  "The connection to Emacs currently in use.")

(defun make-connection (socket stream style)
  (let ((conn (funcall (ecase style
                         (:spawn
                          #'make-multithreaded-connection)
                         ((:sigio nil :fd-handler)
                          #'make-singlethreaded-connection))
                       :socket socket
                       :socket-io stream
                       :communication-style style)))
    (run-hook *new-connection-hook* conn)
    (send-to-sentinel `(:add-connection ,conn))
    conn))

(defslyfun ping (tag)
  tag)

(defun safe-backtrace ()
  (ignore-errors
    (call-with-debugging-environment
     (lambda () (backtrace 0 nil)))))

(define-condition slynk-error (error)
  ((backtrace :initarg :backtrace :reader slynk-error.backtrace)
   (condition :initarg :condition :reader slynk-error.condition))
  (:report (lambda (c s) (princ (slynk-error.condition c) s)))
  (:documentation "Condition which carries a backtrace."))

(defun signal-slynk-error (condition &optional (backtrace (safe-backtrace)))
  (error 'slynk-error :condition condition :backtrace backtrace))

(defvar *debug-on-slynk-protocol-error* nil
  "When non-nil invoke the system debugger on errors that were
signalled during decoding/encoding the wire protocol.  Do not set this
to T unless you want to debug slynk internals.")

(defmacro with-slynk-error-handler ((connection) &body body)
  "Close the connection on internal `slynk-error's."
  (let ((conn (gensym)))
  `(let ((,conn ,connection))
     (handler-case
         (handler-bind ((slynk-error
                         (lambda (condition)
                           (when *debug-on-slynk-protocol-error*
                             (invoke-default-debugger condition)))))
           (progn . ,body))
       (slynk-error (condition)
         (close-connection ,conn
                           (slynk-error.condition condition)
                           (slynk-error.backtrace condition)))))))

(defmacro with-panic-handler ((connection) &body body)
  "Close the connection on unhandled `serious-condition's."
  (let ((conn (gensym)))
    `(let ((,conn ,connection))
       (handler-bind ((serious-condition
                        (lambda (condition)
                          (close-connection ,conn condition (safe-backtrace))
                          (abort condition))))
         . ,body))))

(add-hook *new-connection-hook* 'notify-backend-of-connection)
(defun notify-backend-of-connection (connection)
  (declare (ignore connection))
  (emacs-connected))
