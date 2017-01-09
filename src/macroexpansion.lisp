(in-package :ls-base)
;;;; Macroexpansion

(defvar *macroexpand-printer-bindings*
  '((*print-circle* . nil)
    (*print-pretty* . t)
    (*print-escape* . t)
    (*print-lines* . nil)
    (*print-level* . nil)
    (*print-length* . nil)
    (*print-case* . :downcase))
  "Pretty-pretty bindings to use when expanding macros")

(defun apply-macro-expander (expander string)
  (with-buffer-syntax ()
    (with-bindings *macroexpand-printer-bindings*
      (prin1-to-string (funcall expander (from-string string))))))

(defslyfun slynk-macroexpand-1 (string)
  (apply-macro-expander #'macroexpand-1 string))

(defslyfun slynk-macroexpand (string)
  (apply-macro-expander #'macroexpand string))

(defslyfun slynk-macroexpand-all (string)
  (apply-macro-expander #'macroexpand-all string))

(defslyfun slynk-compiler-macroexpand-1 (string)
  (apply-macro-expander #'compiler-macroexpand-1 string))

(defslyfun slynk-compiler-macroexpand (string)
  (apply-macro-expander #'compiler-macroexpand string))

(defslyfun slynk-expand-1 (string)
  (apply-macro-expander #'expand-1 string))

(defslyfun slynk-expand (string)
  (apply-macro-expander #'expand string))

(defun expand-1 (form)
  (multiple-value-bind (expansion expanded?) (macroexpand-1 form)
    (if expanded?
        (values expansion t)
        (compiler-macroexpand-1 form))))

(defun expand (form)
  (expand-repeatedly #'expand-1 form))

(defun expand-repeatedly (expander form)
  (loop
    (multiple-value-bind (expansion expanded?) (funcall expander form)
      (unless expanded? (return expansion))
      (setq form expansion))))

(defslyfun slynk-format-string-expand (string)
  (apply-macro-expander #'format-string-expand string))

(defslyfun disassemble-form (form)
  (with-buffer-syntax ()
    (with-output-to-string (*standard-output*)
      (let ((*print-readably* nil))
        (disassemble (eval (read-from-string form)))))))
