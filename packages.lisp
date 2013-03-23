
(defpackage #:selenium
  (:use #:common-lisp)
  (:documentation "An interface to Selenium Remote Control.")
  (:export 
   #:*selenium-session*
   #:*selenium-driver-url*
   #:do-get-new-browser-session
   #:do-test-complete
   #:base-error
   #:http-error
   #:execution-error
   #:with-selenium-session))
