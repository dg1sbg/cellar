;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;; =====================================================================

(declaim (optimize (speed 3) (compilation-speed 0) (safety 1) (debug 3)))

(in-package #:net.goenninger.lib.cellar)

(cells:defmd timestamp ()

  (universal-time     :cell nil)
  (internal-real-time :cell nil)

  :universal-time     (cl:get-universal-time)
  :internal-real-time (cl:get-internal-real-time))

(defun make-timestamp ()
  (make-instance 'timestamp))

(defmethod print-object ((self timestamp) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (format-iso8601-time (universal-time self) t))))

(cells:defmd base-class ()
  (:documentation "The base class for all classes in the CCAG Application Framework.")

  (id :cell nil)
  (creation-timestamp :cell nil)
  (creation-machine :cell nil)
  (creation-machine-type :cell nil)

  :id (uuid:make-v4-uuid)
  :creation-timestamp (make-timestamp)
  :creation-machine (machine-instance)
  :creation-machine-type (machine-type)
  :md-name (gensym "BASE-CLASS-INSTANCE-"))

(defmethod creation-info ((self base-class))
  (format nil "~S: Created on ~a on machine ~a."
	  self
	  (creation-timestamp-iso8601 self)
	  (creation-machine self)))

(defmethod creation-timestamp-universal-time ((self base-class))
  (universal-time (creation-timestamp self)))

(defmethod creation-timestamp-rfc2822 ((self base-class))
  (net.telent.date:universal-time-to-rfc-date
   (universal-time (creation-timestamp self))))

(defmethod creation-timestamp-iso8601 ((self base-class))
  (format-iso8601-time (universal-time (creation-timestamp self)) t))
