;;; guix.scm --- Guix package for sl.  Build with: guix build -f guix.scm
;;; Install with: guix package -f guix.scm
(use-modules (guix packages) (guix gexp) (guix build-system asdf)
             ((guix licenses) #:prefix license:)
             (gnu packages lisp) (gnu packages lisp-xyz) (gnu packages lisp-check))

(define %source-dir (dirname (current-filename)))

(define-public sbcl-sl
  (package
    (name "sbcl-sl")
    (version "1.0")
    (source (local-file %source-dir "sl-checkout"
                        #:recursive? #t
                        #:select? (lambda (file stat)
                                    (not (or (string-suffix? ".fasl" file)
                                             (string-contains file "/.git"))))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:asd-systems ''("sl")))
    (inputs (list
                  sbcl-alexandria))
    (synopsis "Wrapper for the lowest common denominator of Sly and Slime")
    (description "Wrapper for the lowest common denominator of Sly and Slime.")
    (home-page "https://github.com/equwal/defsl")
    (license license:gpl3)))

(define-public cl-sl
  (sbcl-package->cl-source-package sbcl-sl))

sbcl-sl
