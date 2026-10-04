(defpackage :sl-system (:use :cl :asdf))
(in-package :sl-system)

(defsystem :sl
  :version      "1.0"
  :description  "Wrapper for the lowest common denominator of Sly and Slime"
  :author       "Spenser Truex <truex@equwal.com>"
  :serial       t
  :license      "GNU GPL v3"
  :components ((:file "package")
               (:file "macrolayer")
               (:file "sl"))
  :weakly-depends-on (:slynk :swank)
  :in-order-to ((test-op (test-op :sl/test))))

(defsystem :sl/test
  :description  "Tests for SL, with fixture forks and with real forks"
  :depends-on (:sl :fiveam :slynk :swank :md5 :eos :cl-utilities :alexandria
               (:feature :sbcl (:require :sb-md5)))
  :components ((:file "test"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam :run!
                                       (uiop:find-symbol* :sl :sl-test))
               (error "The SL tests failed."))))
