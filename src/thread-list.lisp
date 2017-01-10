(in-package :ls-base)
;;;; Thread listing

(defvar *thread-list* ()
  "List of threads displayed in Emacs.  We don't care a about
synchronization issues (yet).  There can only be one thread listing at
a time.")

(defslyfun list-threads ()
  "Return a list (LABELS (ID NAME STATUS ATTRS ...) ...).
LABELS is a list of attribute names and the remaining lists are the
corresponding attribute values per thread.
Example:
  ((:id :name :status :priority)
   (6 \"slynk-indentation-cache-thread\" \"Semaphore timed wait\" 0)
   (5 \"reader-thread\" \"Active\" 0)
   (4 \"control-thread\" \"Semaphore timed wait\" 0)
   (2 \"Slynk Sentinel\" \"Semaphore timed wait\" 0)
   (1 \"listener\" \"Active\" 0)
   (0 \"Initial\" \"Sleep\" 0))"
  (setq *thread-list* (all-threads))
  (when (and *emacs-connection*
             (use-threads-p)
             ;; FIXME: hardcoded thread name
             (equalp (thread-name (current-thread)) "slynk-worker")) 
    (setf *thread-list* (delete (current-thread) *thread-list*)))
  (let* ((plist (thread-attributes (car *thread-list*)))
         (labels (loop for (key) on plist by #'cddr
                       collect key)))
    `((:id :name :status ,@labels)
      ,@(loop for thread in *thread-list*
              for name = (thread-name thread)
              for attributes = (thread-attributes thread)
              collect (list* (thread-id thread)
                             (string name)
                             (thread-status thread)
                             (loop for label in labels
                                   collect (getf attributes label)))))))

(defslyfun quit-thread-browser ()
  (setq *thread-list* nil))

(defun nth-thread (index)
  (nth index *thread-list*))

(defslyfun debug-nth-thread (index)
  (let ((connection *emacs-connection*))
    (interrupt-thread (nth-thread index)
                      (lambda ()
                        (invoke-or-queue-interrupt
                         (lambda ()
                           (with-connection (connection)
                             (simple-break))))))))

(defslyfun kill-nth-thread (index)
  (kill-thread (nth-thread index)))

(defslyfun start-slynk-server-in-thread (index port-file-name)
  "Interrupt the INDEXth thread and make it start a slynk server.
The server port is written to PORT-FILE-NAME."
  (interrupt-thread (nth-thread index)
                    (lambda ()
                      (start-server port-file-name :style nil))))
