
(defpackage #:selenium
  (:use #:common-lisp)
  (:documentation "Selenium is a test tool for web applications. cl-selenium is a Common Lisp interface to Selenium.")
  (:export 
   #:*selenium-session*
   #:*selenium-driver-url*
   #:do-get-new-browser-session
   #:do-test-complete
   #:base-error
   #:http-error
   #:execution-error
   #:with-selenium-session))
