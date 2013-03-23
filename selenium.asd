
(defpackage #:selenium-system (:use #:common-lisp #:asdf))
(in-package #:selenium-system)

(defsystem #:selenium
  :components ((:file "packages")
  	       (:file "iedoc")
	       (:file "selenium"))
  :serial t
  :depends-on (#:drakma
	       #:split-sequence
	       #:puri
	       #:cl-ppcre
	       #:cxml))
