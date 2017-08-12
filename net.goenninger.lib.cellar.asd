;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(declaim (optimize (debug 1) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :net.goenninger.lib.cellar.asdf
  (:use :cl :asdf))

(in-package :net.goenninger.lib.cellar.asdf)

(asdf:defsystem net.goenninger.lib.cellar
  :name "net.goenninger.lib.cellar"
  :version "1.0.0"

  :author "Frank Goenninger <frank.goenninger@goenninger.net>"
  :maintainer "Gönninger B&T UG (haftungsbeschränkt) <support@goenninger.net>"

  :licence "Propietary. All rights reserved by Gönninger B&T. Do not copy without written permission."

  :description "A collection of utilities not found elsewhere."
  :long-description "A collection of utilities gathered from various sources and just put in one place. Contains code from authors throughout the WWW. This code remains copyrigt of the respective authors. Depends on the followoing packages: closer-mop, alexandria, babel, net-telent-date, uuid, uiop, trvivial-backtrace, cells, log4cl."

  :depends-on (:closer-mop
	       :alexandria
	       :babel
	       :net-telent-date
	       :uuid
	       :uiop
	       :trivial-backtrace
	       :cells
	       :log4cl)

  :serial t

  :components
  ((:module src
	    :pathname "src"
	    :components
	    ((:file "defpackage")
	     (:file "globals")
	     (:file "time")
	     (:file "base")
	     (:file "text")
	     (:file "conditions")
	     (:file "logging")
	     (:file "macro-utilities")
	     (:file "misc")
	     (:file "on-lisp")
	     (:file "fsm")
	     ))))

(defmethod perform ((o load-op) (c (eql (find-system :net.goenninger.lib.cellar))))
  (pushnew :net.goenninger.lib.cellar cl:*features*)
  (provide 'net.goenninger.lib.cellar))
