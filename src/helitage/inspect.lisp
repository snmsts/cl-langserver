(in-package :ls-base)
;;;; Inspecting
(defvar-unbound *current-inspector*
    "Current inspector, bound by EVAL-FOR-INSPECTOR, maybe to nil.")

(defvar-unbound *target-inspector*
    "Target inspector, bound by EVAL-FOR-INSPECTOR, maybe to nil.")

(defun current-inspector ()
  (or (and (boundp '*current-inspector*)
           *current-inspector*)
      (find-inspector "default")
      (make-instance 'inspector :name "default")))

(defun target-inspector ()
  (or (and (boundp '*target-inspector*)
           *target-inspector*)
      (current-inspector)))


(defvar *inspector-printer-bindings*
  '((*print-lines*        . 1)
    (*print-right-margin* . 75)
    (*print-pretty*       . t)
    (*print-readably*     . nil)))

(defvar *inspector-verbose-printer-bindings*
  '((*print-escape* . t)
    (*print-circle* . t)
    (*print-array*  . nil)))

(defclass inspector ()
  ((verbose-p :initform nil :accessor inspector-verbose-p)
   (history :initform (make-array 10 :adjustable t :fill-pointer 0) :accessor inspector-%history)
   (name :initarg :name :initform (error "Name this INSPECTOR!") :accessor inspector-name)))

(defmethod print-object ((i inspector) s)
  (print-unreadable-object (i s :type t) 
    (format s "~a/~a" (inspector-name i) (length (inspector-%history i)))))

(defmethod initialize-instance :after ((i inspector) &key name)
  (assert (not (find-inspector name)) nil "Already have an inspector named ~a" name)
  (push i (connection-inspectors *emacs-connection*)))

(defun find-inspector (name)
  (find name (connection-inspectors *emacs-connection*)
        :key #'inspector-name :test #'string=))

(defstruct inspector-state)
(defstruct (istate (:conc-name istate.) (:include inspector-state))
  object
  (parts (make-array 10 :adjustable t :fill-pointer 0))
  (actions (make-array 10 :adjustable t :fill-pointer 0))
  metadata
  content
  serial)

(defun ensure-istate-metadata (o indicator default)
  (with-struct (istate. object metadata) (current-istate)
    (assert (eq object o))
    (let ((data (getf metadata indicator default)))
      (setf (getf metadata indicator) data)
      data)))

(defun current-istate (&optional (inspector (current-inspector)))
  (let* ((history (inspector-%history inspector)))
    (and (plusp (length history))
         (aref history (1- (length history))))))

(defun reset-inspector ()
  (setf (inspector-%history (current-inspector))
        (make-array 10 :adjustable t :fill-pointer 0)))

(defslyfun init-inspector (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY inspection request.")
      (inspect-object (eval (read-from-string string))))))

(defun inspect-object (o)
  (let* ((inspector (target-inspector))
         (history (inspector-%history inspector))
         (istate (make-istate :object o)))
    (vector-push-extend istate history)
    (let ((*current-inspector* inspector))
      ;; HACK! because EMACS-INSPECT may call ENSURE-ISTATE-METADATA
      ;; which expects its object to be the current istate's objects.
      (setf (istate.content istate)
            (emacs-inspect o)))
    (vector-push-extend :break-history history)
    (decf (fill-pointer history))
    (istate>elisp istate)))

(defun istate>elisp (istate)
  (list :title (prepare-title istate)
        :id (assign-index (istate.object istate) (istate.parts istate))
        :content (prepare-range istate 0 500)
        ;; :serial (istate.serial istate)
        ))

(defun prepare-title (istate)
  (if (inspector-verbose-p (current-inspector))
      (with-bindings *inspector-verbose-printer-bindings*
        (to-string (istate.object istate)))
      (with-string-stream (stream :length 200
                                  :bindings *inspector-printer-bindings*)
        (print-unreadable-object
            ((istate.object istate) stream :type t :identity t)))))

(defun prepare-range (istate start end)
  (let* ((range (content-range (istate.content istate) start end))
         (ps (loop for part in range append (prepare-part part istate))))
    (list ps
          (if (< (length ps) (- end start))
              (+ start (length ps))
              (+ end 1000))
          start end)))

(defun prepare-part (part istate)
  (let ((newline '#.(string #\newline)))
    (etypecase part
      (string (list part))
      (cons (destructure-case part
              ((:newline) (list newline))
              ((:value obj &optional str)
               (list (value-part obj str (istate.parts istate))))
              ((:label &rest strs)
               (list (list :label (apply #'cat (mapcar #'string strs)))))
              ((:action label lambda &key (refreshp t))
               (list (action-part label lambda refreshp
                                  (istate.actions istate))))
              ((:line label value)
               (list (princ-to-string label) ": "
                     (value-part value nil (istate.parts istate))
                     newline)))))))

(defun value-part (object string parts)
  (list :value
        (or string (print-part-to-string object))
        (assign-index object parts)))

(defun action-part (label lambda refreshp actions)
  (list :action label (assign-index (list lambda refreshp) actions)))

(defun assign-index (object vector)
  (let ((index (fill-pointer vector)))
    (vector-push-extend object vector)
    index))

(defun print-part-to-string (value)
  (let* ((*print-readably* nil)
         (string (slynk-pprint-to-line value))
         (pos (position value
                        (inspector-%history (current-inspector))
                        :key #'istate.object)))
    (if pos
        (format nil "@~D=~A" pos string)
        string)))

(defun content-range (list start end)
  (typecase list
    (list (let ((len (length list)))
            (subseq list start (min len end))))
    (lcons (llist-range list start end))))

(defslyfun inspector-nth-part (index)
  "Return the current inspector's INDEXth part.
The second value indicates if that part exists at all."
  (let* ((parts (istate.parts (current-istate)))
         (foundp (< index (length parts))))
    (values (and foundp (aref parts index))
            foundp)))

(defslyfun inspector-nth-part-or-lose (index)
  "Return the current inspector's INDEXth part.
The second value indicates if that part exists at all."
  (multiple-value-bind (part foundp)
      (inspector-nth-part index)
    (if foundp part (error "No part with index ~a" index))))

(defslyfun inspect-nth-part (index)
  (with-buffer-syntax ()
    (inspect-object (inspector-nth-part index))))

(defslyfun inspector-range (from to)
  (prepare-range (current-istate) from to))

(defslyfun inspector-call-nth-action (index &rest args)
  (destructuring-bind (fun refreshp) (aref (istate.actions (current-istate)) index)
    (apply fun args)
    (if refreshp
        (inspector-reinspect)
        ;; tell emacs that we don't want to refresh the inspector buffer
        nil)))

(defslyfun inspector-pop ()
  "Inspect the previous object.
Return nil if there's no previous object."
  (with-buffer-syntax ()
    (let* ((history (inspector-%history (current-inspector))))
      (when (> (length history) 1)
        (decf (fill-pointer history))
        (aref history (fill-pointer history))
        (istate>elisp (current-istate))))))

(defslyfun inspector-next ()
  "Inspect the next element in the history of inspected objects.."
  (with-buffer-syntax ()
    (let* ((history (inspector-%history (current-inspector))))
      (when (and (< (fill-pointer history)
                    (array-dimension history 0))
                 (istate-p (aref history (fill-pointer history))))
        (incf (fill-pointer history))
        (istate>elisp (current-istate))))))

(defslyfun inspector-reinspect ()
  (let ((istate (current-istate)))
    (setf (istate.content istate)
          (emacs-inspect (istate.object istate)))
    (istate>elisp istate)))

(defslyfun inspector-toggle-verbose ()
  "Toggle verbosity of inspected object."
  (setf (inspector-verbose-p (current-inspector))
        (not (inspector-verbose-p (current-inspector))))
  (istate>elisp (current-istate)))

(defslyfun inspector-eval (string)
  (let* ((obj (istate.object (current-istate)))
         (context (eval-context obj))
         (form (with-buffer-syntax ((cdr (assoc '*package* context)))
                 (read-from-string string)))
         (ignorable (remove-if #'boundp (mapcar #'car context))))
    (to-string (eval `(let ((* ',obj) (- ',form)
                            . ,(loop for (var . val) in context
                                     unless (constantp var) collect
                                     `(,var ',val)))
                        (declare (ignorable . ,ignorable))
                        ,form)))))

(defslyfun inspector-history ()
  (slynk-pprint-to-line (inspector-%history (current-inspector))))

(defslyfun quit-inspector ()
  (reset-inspector)
  nil)

(defslyfun describe-inspectee ()
  "Describe the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string (istate.object (current-istate)))))

(defslyfun describe-inspector-part (index)
  "Describe part INDEX of the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string (inspector-nth-part index))))

(defslyfun pprint-inspector-part (index)
  "Pretty-print part INDEX of the currently inspected object."
  (with-buffer-syntax ()
    (slynk-pprint (inspector-nth-part index))))

(defslyfun inspect-in-frame (string index)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY inspection request.")
      (reset-inspector)
      (inspect-object (eval-in-frame (from-string string) index)))))

(defslyfun inspect-current-condition ()
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object *slynk-debugger-condition*)))

(defslyfun inspect-frame-var (frame var)
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object (frame-var-value frame var))))

(defslyfun pprint-frame-var (frame var)
  (with-buffer-syntax ()
    (slynk-pprint (frame-var-value frame var))))

(defslyfun describe-frame-var (frame var)
  (with-buffer-syntax ()
    (describe-to-string (frame-var-value frame var))))

(defslyfun eval-for-inspector (current
                               target
                               slave-slyfun &rest args)
  "Call SLAVE-SLYFUN with ARGS in CURRENT inspector, open in TARGET."
  (let ((*current-inspector* (and current
                                  (or (find-inspector current)
                                      (make-instance 'inspector :name current))))
        (*target-inspector* (and target
                                 (or (find-inspector target)
                                     (make-instance 'inspector :name target)))))
    (apply slave-slyfun args)))

;;;;; Lists

(defmethod emacs-inspect ((o cons))
  (if (listp (cdr o))
      (inspect-list o)
      (inspect-cons o)))

(defun inspect-cons (cons)
  (label-value-line*
   ('car (car cons))
   ('cdr (cdr cons))))

(defun inspect-list (list)
  (multiple-value-bind (length tail) (safe-length list)
    (flet ((frob (title list)
             (list* title '(:newline) (inspect-list-aux list))))
      (cond ((not length)
             (frob "A circular list:"
                   (cons (car list)
                         (ldiff (cdr list) list))))
            ((not tail)
             (frob "A proper list:" list))
            (t
             (frob "An improper list:" list))))))

(defun inspect-list-aux (list)
  (loop for i from 0  for rest on list  while (consp rest)  append
        (if (listp (cdr rest))
            (label-value-line i (car rest))
            (label-value-line* (i (car rest)) (:tail (cdr rest))))))

(defun safe-length (list)
  "Similar to `list-length', but avoid errors on improper lists.
Return two values: the length of the list and the last cdr.
Return NIL if LIST is circular."
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast list (cddr fast))          ;Fast pointer: leaps by 2.
       (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
      (nil)
    (cond ((null fast) (return (values n nil)))
          ((not (consp fast)) (return (values n fast)))
          ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
          ((and (eq fast slow) (> n 0)) (return nil))
          ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

;;;;; Hashtables

(defun hash-table-to-alist (ht)
  (let ((result '()))
    (maphash (lambda (key value)
               (setq result (acons key value result)))
             ht)
    result))

(defmethod emacs-inspect ((ht hash-table))
  (append
   (label-value-line*
    ("Count" (hash-table-count ht))
    ("Size" (hash-table-size ht))
    ("Test" (hash-table-test ht))
    ("Rehash size" (hash-table-rehash-size ht))
    ("Rehash threshold" (hash-table-rehash-threshold ht)))
   (let ((weakness (hash-table-weakness ht)))
     (when weakness
       (label-value-line "Weakness:" weakness)))
   (unless (zerop (hash-table-count ht))
     `((:action "[clear hashtable]"
                ,(lambda () (clrhash ht))) (:newline)
       "Contents: " (:newline)))
   (let ((content (hash-table-to-alist ht)))
     (cond ((every (lambda (x) (typep (first x) '(or string symbol))) content)
            (setf content (sort content 'string< :key #'first)))
           ((every (lambda (x) (typep (first x) 'number)) content)
            (setf content (sort content '< :key #'first))))
     (loop for (key . value) in content appending
           `((:value ,key) " = " (:value ,value)
             " " (:action "[remove entry]"
                          ,(let ((key key))
                                (lambda () (remhash key ht))))
             (:newline))))))

;;;;; Arrays

(defmethod emacs-inspect ((array array))
  (lcons*
   (iline "Dimensions" (array-dimensions array))
   (iline "Element type" (array-element-type array))
   (iline "Total size" (array-total-size array))
   (iline "Adjustable" (adjustable-array-p array))
   (iline "Fill pointer" (if (array-has-fill-pointer-p array)
                             (fill-pointer array)))
   "Contents:" '(:newline)
   (labels ((k (i max)
              (cond ((= i max) '())
                    (t (lcons (iline i (row-major-aref array i))
                              (k (1+ i) max))))))
     (k 0 (array-total-size array)))))

;;;;; Chars

(defmethod emacs-inspect :around (object)
  (declare (ignore object))
  (with-bindings (if (inspector-verbose-p (current-inspector))
                     *inspector-verbose-printer-bindings*
                     *inspector-printer-bindings*)
    (call-next-method)))

(defmethod emacs-inspect ((char character))
  (append
   (label-value-line*
    ("Char code" (char-code char))
    ("Lower cased" (char-downcase char))
    ("Upper cased" (char-upcase char)))
   (if (get-macro-character char)
       `("In the current readtable ("
         (:value ,*readtable*) ") it is a macro character: "
         (:value ,(get-macro-character char))))))
