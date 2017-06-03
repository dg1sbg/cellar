;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;; =====================================================================

(declaim (optimize (speed 3) (compilation-speed 0) (safety 1) (debug 3)))

(in-package #:net.goenninger.lib.cellar)

(let ((log-available-p (if (find-package "LOG")
			   t
			   nil)))
  (defun logit (level format-ctrl &rest args)
    (let ((msg (apply #'format nil format-ctrl args)))
      (if log-available-p
	  (case level
	    (:trace (log:trace msg))
	    (:debug (log:debug msg))
	    (:info  (log:info  msg))
	    (:warn  (log:warn  msg))
	    (:error (log:error msg))
	    (:fatal (log:fatal msg))
	    (otherwise (format *debug-io* "~&~a~&" msg)))
	  (format *debug-io* "~&~a~&" msg)))))
