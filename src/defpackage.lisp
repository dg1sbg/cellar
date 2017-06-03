;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :cl-user)

(defpackage #:net.goenninger.lib.cellar
  (:nicknames :cellar)
  (:use :cl)
  (:export

   ;; CLASSES

   #:base-class

   ;; FUNCTIONS & MACROS

   ;; On Lisp

   #:var?
   #:varsym?
   #:vars-in
   #:simple?
   #:destruct
   #:single
   #:append1
   #:conc1
   #:mklist
   #:longer
   #:filter
   #:group
   #:prune
   ;;....

   ;; Globals

   #:defv
   #:defc
   #:deflexical
   #:constant?

   ;; Misc

   #:defdbgobserver
   #:define-config-var
   #:alternate-functions
   #:string-join
   #:argv0
   #:export!
   #:getenv
   #:intern-with-package$
   #:intern-as-keyword$
   #:stringify
   #:muffle-redefinition-warnings

   ;; fsm

   #:standard-state-machine
   #:standard-state-machine-event
   #:state
   #:defstate
   #:deffsm

   ))
