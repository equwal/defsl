(defpackage :sl-system (:use :cl :asdf))
(in-package :sl-system)

(defsystem :sl
  :version      "1.0"
  :description  "Wrapper for the lowest common denominator of Sly and Slime"
  :author       "Spenser Truex <truex@equwal.com>"
  :serial       t
  :license      "GNU GPL v3"
  :components ((:file "package")
               (:file "utils")
               (:file "macrolayer")
               (:file "sl"))
  :weakly-depends-on (:slynk :swank)
  :depends-on (:alexandria))
