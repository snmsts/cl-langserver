(in-package :ls-base)

;;;;; Sentinel
;;;
;;; The sentinel thread manages some global lists.
;;; FIXME: Overdesigned?

(defvar *connections* '()
  "List of all active connections, with the most recent at the front.")

(defvar *servers* '()
  "A list ((server-socket port thread) ...) describing the listening sockets.
Used to close sockets on server shutdown or restart.")

;; FIXME: we simply access the global variable here.  We could ask the
;; sentinel thread instead but then we still have the problem that the
;; connection could be closed before we use it.
(defun default-connection ()
  "Return the 'default' Emacs connection.
This connection can be used to talk with Emacs when no specific
connection is in use, i.e. *EMACS-CONNECTION* is NIL.

The default connection is defined (quite arbitrarily) as the most
recently established one."
  (car *connections*))

(defun start-sentinel ()
  (unless (find-registered 'sentinel)
    (let ((thread (spawn #'sentinel :name "Slynk Sentinel")))
      (register-thread 'sentinel thread))))

(defun sentinel ()
  (catch 'exit-sentinel
    (loop (sentinel-serve (receive)))))

(defun send-to-sentinel (msg)
  (let ((sentinel (find-registered 'sentinel)))
    (cond (sentinel (send sentinel msg))
          (t (sentinel-serve msg)))))

(defun sentinel-serve (msg)
  (destructure-case msg
    ((:add-connection conn)
     (push conn *connections*))
    ((:close-connection connection condition backtrace)
     (close-connection% connection condition backtrace)
     (sentinel-maybe-exit))
    ((:add-server socket port thread)
     (push (list socket port thread) *servers*))
    ((:stop-server key port)
     (sentinel-stop-server key port)
     (sentinel-maybe-exit))))

(defun sentinel-stop-server (key value)
  (let ((probe (find value *servers* :key (ecase key
                                            (:socket #'car)
                                            (:port #'cadr)))))
    (cond (probe
           (setq *servers* (delete probe *servers*))
           (destructuring-bind (socket _port thread) probe
             (declare (ignore _port))
             (ignore-errors (close-socket socket))
             (when (and thread
                        (thread-alive-p thread)
                        (not (eq thread (current-thread))))
               (kill-thread thread))))
          (t
           (warn "No server for ~s: ~s" key value)))))

(defun sentinel-maybe-exit ()
  (when (and (null *connections*)
             (null *servers*)
             (and (current-thread)
                  (eq (find-registered 'sentinel)
                      (current-thread))))
    (register-thread 'sentinel nil)
    (throw 'exit-sentinel nil)))
