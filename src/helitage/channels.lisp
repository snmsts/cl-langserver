(in-package :ls-base)


;;; Channels

(defmacro channels () `(connection-channels *emacs-connection*))
(defmacro channel-counter () `(connection-channel-counter *emacs-connection*))

(defclass channel ()
  ((id     :initform (incf (channel-counter))
           :reader channel-id)
   (thread :initarg :thread :initform (current-thread)
           :reader channel-thread)
   (name   :initarg :name   :initform nil)))

(defmethod initialize-instance :after ((ch channel) &key)
  ;; FIXME: slightly fugly, but I need this to be able to name the
  ;; thread according to the channel's id.
  ;;
  (with-slots (thread) ch
    (when (use-threads-p)
      (setf thread (spawn-channel-thread *emacs-connection* ch)))
    (ls-backend:send thread `(:serve-channel ,ch)))
  (setf (channels) (nconc (channels) (list ch))))

(defmethod print-object ((c channel) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (id name) c
      (format stream "~d ~a" id name))))

(defmethod drop-unprocessed-events (channel)
  ;; FIXME: perhaps this should incorporate most
  ;; behaviour from it's :after spec currently in slynk-mrepl.lisp)
  (declare (ignore channel)))

(defun find-channel (id)
  (find id (channels) :key #'channel-id))

(defun find-channel-thread (channel)
  (channel-thread channel))

(defun channel-thread-id (channel)
  (ls-backend:thread-id (channel-thread channel)))

(defmethod close-channel (channel)
  (let ((probe (find-channel (channel-id channel))))
    (cond (probe (setf (channels) (delete probe (channels))))
          (t (error "Can't close invalid channel: ~a" channel)))))

(defgeneric channel-send (channel selector args)
  (:documentation "Send to CHANNEL the message SELECTOR with ARGS."))

(defmacro define-channel-method (selector (channel &rest args) &body body)
  `(defmethod channel-send (,channel (selector (eql ',selector)) args)
     (destructuring-bind ,args args
       . ,body)))

(define-channel-method :teardown ((c channel))
  (if (use-threads-p)
      ;; eventually calls CLOSE-CHANNEL
      (throw 'stop-processing 'listener-teardown)
      (close-channel c)))

(defun send-to-remote-channel (channel-id msg)
  (send-to-emacs `(:channel-send ,channel-id ,msg)))
