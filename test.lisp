;;; Tests for DEFSL. Run them with (asdf:test-system :sl).

(defpackage #:sl-test.a (:use))
(defpackage #:sl-test.b (:use))
(defpackage #:sl-test.c (:use))

(defpackage #:sl-test
  (:use #:cl #:fiveam #:sl))
(in-package #:sl-test)

(def-suite sl :description "DEFSL takes the first available definition.")
(in-suite sl)

;;; Fixture forks. Each test decides which of them define GREET.

(defparameter *forks* '(sl-test.a sl-test.b sl-test.c)
  "The fixture packages, in the order of preference that the tests use.")

(defun provide-greet (available)
  "Define GREET in each AVAILABLE fork, and in no other fork."
  (dolist (fork *forks*)
    (let ((greet (intern "GREET" fork)))
      (if (member fork available)
          (setf (symbol-function greet) (constantly fork))
          (fmakunbound greet)))))

(test first-available-fork-wins
  "For each subset of forks that define GREET, DEFSL takes the first one."
  (dotimes (mask (expt 2 (length *forks*)))
    (let ((available (loop for fork in *forks*
                           for bit from 0
                           when (logbitp bit mask) collect fork)))
      (provide-greet available)
      (fmakunbound 'greet)
      (if available
          (progn (defsl greet :fn :eq sl-test.a sl-test.b sl-test.c)
                 (is (eq (first available) (funcall 'greet))))
          (signals error
            (defsl greet :fn :eq sl-test.a sl-test.b sl-test.c))))))

(test missing-package-is-skipped
  (provide-greet '(sl-test.b))
  (is (eq 'greet (defsl greet :fn :eq sl-test.no-such-fork sl-test.b)))
  (is (eq 'sl-test.b (funcall 'greet))))

(test qualified-names
  "Each fork can use its own name. DEFSL interns no symbol to look one up."
  (provide-greet '(sl-test.b))
  (defsl hello :fn sl-test.a salute sl-test.b greet)
  (is (eq 'sl-test.b (funcall 'hello)))
  (is (null (find-symbol "SALUTE" 'sl-test.a))))

(test variables
  (setf (symbol-value (intern "*MOTTO*" 'sl-test.b)) "fork b")
  (defsl *motto* :sym :eq sl-test.a sl-test.b)
  (is (equal "fork b" (symbol-value '*motto*))))

(test malformed-spec
  "A package without a symbol is an error when the macro expands."
  (signals error (macroexpand '(defsl greet :fn sl-test.a greet sl-test.b))))

;;; Real forks. The test system loads both sides of each fork, except MICROS,
;;; so the first preference wins.

(defsl operator-arglist :fn :eq slynk swank micros)

(test slime-forks
  "Sly's SLYNK comes before SLIME's SWANK and Lem's MICROS."
  (is (eq #'operator-arglist #'slynk:operator-arglist))
  (is (stringp (operator-arglist "car" "CL"))))

(defsl md5sum-string :fn :eq sb-md5 md5)

(test md5-forks
  "SB-MD5 is SBCL's fork of MD5. Other implementations fall back to MD5."
  (is (equalp #(144 1 80 152 60 210 79 176 214 150 63 125 40 225 127 114)
              (md5sum-string "abc"))))

(defsl check (macro-function macro-function) eos is fiveam is)

(test fiveam-forks
  "Eos is a fork of FiveAM. A list names the namespace of macros."
  (is (eq (macro-function 'check) (macro-function 'eos:is))))

(defsl gensymmer (macro-function macro-function)
  cl-utilities with-unique-names
  alexandria with-gensyms)

(test renamed-macro
  "The alias exists at compile time, so this file can use it as a macro."
  (is (eq (macro-function 'gensymmer)
          (macro-function 'cl-utilities:with-unique-names)))
  (is (symbolp (gensymmer (name) name))))
