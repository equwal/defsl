;;; DEFSL gives a name to the first available definition among forks.
;;;
;;; The macro only parses its arguments. RESOLVE makes the choice when the
;;; expansion runs, from strings. Thus the reader needs none of the packages.

(in-package :sl)

(defun namespace (designator)
  "Return the accessor and the bound-predicate that DESIGNATOR names."
  (case designator
    (:fn '(symbol-function fboundp))
    (:sym '(symbol-value boundp))
    (t designator)))

(defun candidates (name spec)
  "Return the (package-name symbol-name) pairs of SPEC, best first."
  (cond ((eq (first spec) :eq)
         (loop for package in (rest spec)
               collect (list (string package) (string name))))
        ((evenp (length spec))
         (loop for (package symbol) on spec by #'cddr
               collect (list (string package) (string symbol))))
        (t (error "DEFSL ~S: a package in ~S has no symbol." name spec))))

(defun resolve (name boundp candidates)
  "Return the first of CANDIDATES that is a symbol which satisfies BOUNDP."
  (loop for (package symbol-name) in candidates
        for symbol = (and (find-package package)
                          (find-symbol symbol-name package))
        when (and symbol (funcall boundp symbol))
          return symbol
        finally (error "DEFSL ~S: no definition is available in ~
                        ~{~{~A:~A~}~^, ~}."
                       name candidates)))

(defmacro defsl (name namespace &rest spec)
  "Define NAME as the first available definition that SPEC lists.

NAMESPACE is :FN for a function, :SYM for a variable, or a list
(ACCESSOR BOUNDP). ACCESSOR is a SETF-able function of a symbol. BOUNDP is a
predicate of a symbol.

SPEC is :EQ and the packages that share NAME, or pairs of a package and a
symbol. A candidate is available if its package exists, the package has the
symbol, and the symbol satisfies BOUNDP.

  (defsl operator-arglist :fn :eq slynk swank)
  (defsl gensymmer (macro-function macro-function)
    utils with-unique-names
    alexandria with-gensyms)

The definition exists at compile time, so a file can use an alias of a macro
that it defines."
  (destructuring-bind (accessor boundp) (namespace namespace)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (,accessor ',name)
             (,accessor (resolve ',name #',boundp ',(candidates name spec))))
       ',name)))
