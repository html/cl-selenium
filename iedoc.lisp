
(in-package #:selenium)

(defclass iedoc-function ()
  ((name :initarg :name
	 :accessor iedoc-function-name)
   (parameters :initarg :parameters
	       :accessor iedoc-function-parameters)
   (comment :initarg :comment
	    :accessor iedoc-function-comment)
   (return-type :initarg :return-type
                :accessor iedoc-return-type)))

(defmethod print-object ((iedoc-function iedoc-function) stream)
  (print-unreadable-object (iedoc-function stream :identity t :type t)
    (format stream "~A(~D)"
	    (iedoc-function-name iedoc-function)
	    (length (iedoc-function-parameters iedoc-function)))))

(defclass iedoc-parameter ()
  ((name :initarg :name
	 :accessor iedoc-parameter-name)
   (comment :initarg :comment
	    :accessor iedoc-parameter-comment)))

(defmethod print-object ((iedoc-parameter iedoc-parameter) stream)
  (print-unreadable-object (iedoc-parameter stream :identity t :type t)
    (format stream "~A" (iedoc-parameter-name iedoc-parameter))))

(defun make-iedoc-function (e)
  (let ((name (dom:get-attribute e "name")))
    (make-instance 'iedoc-function
		   :name name
		   :parameters (loop for p across (dom:get-elements-by-tag-name e "param")
				  collect (make-iedoc-parameter p))
		   :comment (make-iedoc-comment (elt (dom:get-elements-by-tag-name e "comment") 0))
                   :return-type (let ((return-elements (dom:get-elements-by-tag-name e "return")))
                                  (when (plusp (length return-elements))
                                    (make-iedoc-return-type (elt return-elements 0)))))))

(defun make-iedoc-parameter (e)
  (let ((name (dom:get-attribute e "name")))
    (make-instance 'iedoc-parameter
		   :name name
		   :comment (make-iedoc-comment e))))

(defun make-iedoc-comment (e)
  (labels ((make-iedoc-comment-r (e stream)
	     (loop for i across (dom:child-nodes e)
		do (if (dom:text-node-p i)
		       (write-string (dom:data i) stream)
		       (make-iedoc-comment-r i stream)))))
    (normalize-comment
     (with-output-to-string (stream)
       (make-iedoc-comment-r e stream)))))

(defun make-iedoc-return-type (e)
  (let ((type (dom:get-attribute e "type")))
    (cond
      ((string-equal type "string") 'string)
      ((string-equal type "boolean") 'boolean)
      ((string-equal type "number") 'number)
      ((string-equal type "string[]") 'list))))


(defun normalize-comment (comment)
  (cl-ppcre:regex-replace-all " {3,}" (cl-ppcre:regex-replace-all "[\\t\\n]" comment " ") " "))

(defun parse-iedoc (pathname)
  (let ((document (cxml:parse-file pathname (cxml-dom:make-dom-builder))))
    (loop for e across (dom:get-elements-by-tag-name document "function")
       collect (make-iedoc-function e))))

(defun convert-function-name (name)
  (intern (concatenate 'string "DO-" (string-upcase (cl-ppcre:regex-replace-all "([A-Z])" name "-\\1"))) "SELENIUM"))

(defun convert-parameter-name (name)
  (intern (string-upcase (cl-ppcre:regex-replace-all "([A-Z])" (cl-ppcre:regex-replace-all "ID" name "Id") "-\\1"))))

(defun convert-parameters (iedoc-function-parameters)
  (mapcar #'(lambda (parameter)
	      (convert-parameter-name (iedoc-parameter-name parameter)))
	  iedoc-function-parameters))

(defun marshall-request (command &rest parameters)
  (cons `("cmd" . ,command)
        (let ((argument-count 0))
          (mapcar #'(lambda (parameter)
                      (cons (format nil "~D" (incf argument-count))
                            (format nil "~A" parameter)))
                  parameters))))

(defun starts-with (s prefix)
  "Returns t if s starts with prefix"
  (let ((mismatch (mismatch s prefix
                            :test #'char=)))
    (or (null mismatch)
        (>= mismatch (length prefix)))))

(defun convert-result (s return-type)
  (ecase return-type
    ((number string) s)
    (boolean (cond
               ((string= s "true") t)
               ((string= s "false") nil)))
    (list (convert-string-array s))
    ((nil) nil)))

;;; Shamelessly copied from selenium.rb in selenium-ruby-client-driver
(defun convert-string-array (s)
  (flet ((new-string ()
           (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (let ((result nil)
          (escape-p nil)
          (token (new-string)))
      (loop
         for char across s
         do (cond
              (escape-p
               (vector-push-extend char token)
               (setf escape-p nil))
              ((eql char #\\)
               (setf escape-p t))
              ((eql char #\,)
               (push token result)
               (setf token (new-string)))
              (t
               (vector-push-extend char token)))
         finally (push token result))
      (nreverse result))))

(define-condition base-error (error)
  ())

(define-condition http-error (base-error)
  ((status-code :reader status-code
                :initarg :status-code)
   (reason :reader reason
           :initarg :reason))
  (:report (lambda (c s)
             (format s "HTTP Error(~d): ~A" 
                     (status-code c) 
                     (reason c)))))

(define-condition execution-error (base-error)
  ((description :reader description
                :initarg :description))
  (:report (lambda (c s)
             (format s "Selenium execution error: ~A" 
                     (description c)))))

(defun execute (url parameters &optional return-type)
  (multiple-value-bind (reply status-code headers reply-from stream some-bool reason)
      (drakma:http-request url :method :get :parameters parameters)
    (declare (ignore headers reply-from stream some-bool))
    (when (/= 200 status-code)
      (error 'http-error :status-code status-code :reason reason))
    (cond
      ((starts-with reply "OK")
       (convert-result (subseq reply (min 3 (length reply))) return-type))
      ((starts-with reply "ERROR:")
       (let ((err (second (split-sequence:split-sequence #\: reply))))
         (error 'execution-error :description err))))))

(defun convert-function (iedoc-function)
  (let ((function-name (convert-function-name (iedoc-function-name iedoc-function)))
	(parameters (convert-parameters (iedoc-function-parameters iedoc-function))))
    `(defun ,function-name
	 (,@parameters &optional (session *selenium-session*))
       ,(iedoc-function-comment iedoc-function)
       (let ((parameters (cons `("sessionId" . ,session)
                               (marshall-request ,(iedoc-function-name iedoc-function) ,@parameters))))
         (execute *selenium-driver-url*
                  parameters
                  ',(iedoc-return-type iedoc-function))))))

#+nil (mapcar #'convert-function (parse-iedoc #p"/home/mkennedy/src/cl-selenium/iedoc-0.8.3-1879.xml"))

(defvar *selenium-driver-url* (puri:parse-uri "http://localhost:4444/selenium-server/driver")
  "The URL of the Selenium Remote Control server.")

(defvar *selenium-session* nil
  "Almost all functions take an optional argument called session which
  defaults to this special variable.  You can bind a selenium session
  to this variable and use those functions without providing an
  explicit session each time.")
