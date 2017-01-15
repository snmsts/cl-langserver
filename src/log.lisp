(in-package :langserver-base)

;;;;; Logging

(defvar *langserver-io-package*
  (let ((package (make-package :ls-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defvar *log-events* nil)
(defvar *log-output* nil) ; should be nil for image dumpers

(defun init-log-output ()
  (unless *log-output*
    (setq *log-output* (real-output-stream *error-output*))))

(add-hook *after-init-hook* 'init-log-output)

(defun real-input-stream (stream)
  (typecase stream
    (synonym-stream
     (real-input-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (real-input-stream (two-way-stream-input-stream stream)))
    (t stream)))

(defun real-output-stream (stream)
  (typecase stream
    (synonym-stream
     (real-output-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (real-output-stream (two-way-stream-output-stream stream)))
    (t stream)))

(defvar *event-history* (make-array 40 :initial-element nil)
  "A ring buffer to record events for better error messages.")
(defvar *event-history-index* 0)
(defvar *enable-event-history* t)

(defun log-event (format-string &rest args)
  "Write a message to *terminal-io* when *log-events* is non-nil.
Useful for low level debugging."
  (with-standard-io-syntax
    (let ((*print-readably* nil)
          (*print-pretty* nil)
          (*package* *langserver-io-package*))
      (when *enable-event-history*
        (setf (aref *event-history* *event-history-index*)
              (format nil "~?" format-string args))
        (setf *event-history-index*
              (mod (1+ *event-history-index*) (length *event-history*))))
      (when *log-events*
        (write-string (escape-non-ascii (format nil "~?" format-string args))
                      *log-output*)
        (force-output *log-output*)))))

(defun event-history-to-list ()
  "Return the list of events (older events first)."
  (let ((arr *event-history*)
        (idx *event-history-index*))
    (concatenate 'list (subseq arr idx) (subseq arr 0 idx))))

(defun clear-event-history ()
  (fill *event-history* nil)
  (setq *event-history-index* 0))

(defun dump-event-history (stream)
  (dolist (e (event-history-to-list))
    (dump-event e stream)))

(defun dump-event (event stream)
  (cond ((stringp event)
         (write-string (escape-non-ascii event) stream))
        ((null event))
        (t
         (write-string
          (escape-non-ascii (format nil "Unexpected event: ~A~%" event))
          stream))))

(defun escape-non-ascii (string)
  "Return a string like STRING but with non-ascii chars escaped."
  (cond ((ascii-string-p string) string)
        (t (with-output-to-string (out)
             (loop for c across string do
               (cond ((ascii-char-p c) (write-char c out))
                     (t (format out "\\x~4,'0X" (char-code c)))))))))

(defun ascii-string-p (o)
  (and (stringp o)
       (every #'ascii-char-p o)))

(defun ascii-char-p (c)
  (<= (char-code c) 127))
