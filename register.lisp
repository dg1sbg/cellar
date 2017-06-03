;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  #+quicklisp
  (progn
    (when (ql:quickload :log4cl)
      (pushnew :log4cl cl:*features*))
    (ql:quickload :cl-fad)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (let* ((truename *load-pathname*)
	 (path (if truename
		   (cl-fad:canonical-pathname (cl-fad:merge-pathnames-as-directory truename))
		   nil)))
    (when path
      (pushnew path asdf:*central-registry* :test #'eql)
      #+log4cl
      (log:info "Path ~a registered in ASDF central registry." path))))
