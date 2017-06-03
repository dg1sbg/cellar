;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-

(in-package #:net.goenninger.lib.cellar)

;;; This has been taken from
;;; http://uint32t.blogspot.de/2007/12/restful-handlers-with-hunchentoot.html
;;; (Created in 2007 by Sohail Somani)

(defun create-regex-dispatcher (regex page-function)
  "Just like tbnl:create-regex-dispatcher except it extracts the matched values
   and passes them onto PAGE-FUNCTION as arguments. You want to be explicit about
   where the slashes go.

   For example, given:
   (defun blog-page (pagenum)
     ... )

   (push (create-regex-dispatcher \"^/blog/page(\\d+)\" #'blog-page)
         tbnl:*dispatch-table*)

   When the url blog/page5 is accessed, blog-page is called with pagenum
   set to 5."
  (let ((scanner (cl-ppcre:create-scanner regex)))
    (lambda (request)
      (multiple-value-bind (whole-match matched-registers)
          (cl-ppcre:scan-to-strings scanner (tbnl:script-name request))
        (when whole-match
          (lambda ()
            (apply page-function (coerce matched-registers 'list))))))))

;;; If there is no match, id will be the empty string.
;;; Should be a better way to handle this, but OK for now.
(defun resource-function (&optional id)
           (cl-who:with-html-output-to-string(s)
             (:html
              (:body
               (tbnl:log-message* "~A ~A")
               (if (or (null id)
                       (= 0 (length id)))
                   (cl-who:htm
                    (cl-who:str "No resource"))
                   (cl-who:htm
                    (cl-who:fmt "Resource ~A" id)))))))

(defun register-resources-dispatcher ()
  (push (create-regex-dispatcher "^/resources/(\\d*)" #'resource-function)
	tbnl:*dispatch-table*))


