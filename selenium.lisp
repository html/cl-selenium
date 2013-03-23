
(in-package #:selenium)

(defmacro define-iedoc (pathname)
  (let ((functions (parse-iedoc pathname)))
    `(progn
       ,@(mapcar #'convert-function functions)
       ,@(mapcar (lambda (fn)
                   `(export (convert-function-name ,(iedoc-function-name fn)) 'selenium))
                 functions))))

(define-iedoc
    #.(merge-pathnames #p"iedoc-0.8.3-1879.xml"
		       *compile-file-pathname*))

(defun do-get-new-browser-session (browser url)
  "Create a session by using the the given browser and initial URL."
  (execute *selenium-driver-url*
           (marshall-request "getNewBrowserSession" browser url)
           'string))

(defun do-test-complete (&optional (session *selenium-session*))
  "Destroy session, closing the browser."
  (execute *selenium-driver-url* 
           (cons `("sessionId" . ,session)
                 (marshall-request "testComplete"))))

(defmacro with-selenium-session ((var browser url) &body body)
  "Evaluate BODY within a Selenium RC session specified by VAR.
Once the body is evaluated, the test complete command is sent which
closes the session and browser."
  `(let ((,var (do-get-new-browser-session ,browser ,url)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var
         (do-test-complete ,var)))))
