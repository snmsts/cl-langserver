(in-package :langserver-base)

;;;;; Symbols

;; FIXME: this docstring is more confusing than helpful.
(defun symbol-status (symbol &optional (package (symbol-package symbol)))
  "Returns one of

  :INTERNAL  if the symbol is _present_ in PACKAGE as an _internal_ symbol,

  :EXTERNAL  if the symbol is _present_ in PACKAGE as an _external_ symbol,

  :INHERITED if the symbol is _inherited_ by PACKAGE through USE-PACKAGE,
             but is not _present_ in PACKAGE,

  or NIL     if SYMBOL is not _accessible_ in PACKAGE.


Be aware not to get confused with :INTERNAL and how \"internal
symbols\" are defined in the spec; there is a slight mismatch of
definition with the Spec and what's commonly meant when talking
about internal symbols most times. As the spec says:

  In a package P, a symbol S is

     _accessible_  if S is either _present_ in P itself or was
                   inherited from another package Q (which implies
                   that S is _external_ in Q.)

        You can check that with: (AND (SYMBOL-STATUS S P) T)


     _present_     if either P is the /home package/ of S or S has been
                   imported into P or exported from P by IMPORT, or
                   EXPORT respectively.

                   Or more simply, if S is not _inherited_.

        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS
                                        (NOT (EQ STATUS :INHERITED))))


     _external_    if S is going to be inherited into any package that
                   /uses/ P by means of USE-PACKAGE, MAKE-PACKAGE, or
                   DEFPACKAGE.

                   Note that _external_ implies _present_, since to
                   make a symbol _external_, you'd have to use EXPORT
                   which will automatically make the symbol _present_.

        You can check that with: (EQ (SYMBOL-STATUS S P) :EXTERNAL)


     _internal_    if S is _accessible_ but not _external_.

        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS
                                        (NOT (EQ STATUS :EXTERNAL))))


        Notice that this is *different* to
                                 (EQ (SYMBOL-STATUS S P) :INTERNAL)
        because what the spec considers _internal_ is split up into two
        explicit pieces: :INTERNAL, and :INHERITED; just as, for instance,
        CL:FIND-SYMBOL does.

        The rationale is that most times when you speak about \"internal\"
        symbols, you're actually not including the symbols inherited
        from other packages, but only about the symbols directly specific
        to the package in question.
"
  (when package     ; may be NIL when symbol is completely uninterned.
    (check-type symbol symbol) (check-type package package)
    (multiple-value-bind (present-symbol status)
        (find-symbol (symbol-name symbol) package)
      (and (eq symbol present-symbol) status))))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."
  (eq (symbol-status symbol package) :external))

(defun baroque-symbol-name-p (symbol)
  (or (> (length (symbol-name symbol)) 60)))

(defparameter *exclude-symbol-functions*
  '(baroque-symbol-name-p)
  "Functions excluding symbols from completion.
Holds a list of boolean predicates of a single argument, a symbol")

(defun excluded-from-searches-p (symbol)
  "Tell if SYMBOL should be excluded from \"apropos\" or completion."
  (some (lambda (fn) (funcall fn symbol)) *exclude-symbol-functions*))
