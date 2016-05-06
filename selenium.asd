
(defpackage #:selenium-system (:use #:common-lisp #:asdf))
(in-package #:selenium-system)

(defsystem #:selenium
  :author "Matthew Kennedy"
  :version "0.4.1"
  :license "LLGPL"
  :description "Selenium is a test tool for web applications. cl-selenium is a Common Lisp interface to Selenium."
  :components ((:file "packages")
  	       (:file "iedoc")
	       (:file "selenium"))
  :serial t
  :depends-on (#:drakma
	       #:split-sequence
	       #:puri
	       #:cl-ppcre
	       #:cxml))
