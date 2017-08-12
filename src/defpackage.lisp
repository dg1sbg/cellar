;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :cl-user)

(defpackage "NET.GOENNINGER.LIB.CELLAR"
  (:nicknames "CELLAR")
  (:use  "CL")
  (:export

   ;; CLASSES, STRUCTS & TYPES

   #:base-class
   #:rds-info
   #:category-nr-type
   #:rds-object-info
   #:msg-info
   #:msg
   #:language-desc
   #:text

   ;; CONDITIONS

   #:cellar-condition

   #:debug-condition
   #:simple-debug-condition

   #:info-condition
   #:simple-info-condition

   #:notice-condition
   #:simple-notice-condition

   #:warning-condition
   #:simple-warning-condition

   #:error-condition
   #:simple-error-condition

   #:critical-condition
   #:simple-critical-condition

   #:alert-condition
   #:simple-alert-condition

   #:emergency-condition
   #:simple-emergency-condition

   ;; VARIABLES & CONSTANTS

   #:*catch-errors-p*
   #:*invoke-debugger-p*
   #:*print-backtrace-p*
   #:log-using-log4cl-p*

   #:+severity-level-emergency+
   #:+severity-level-alert+
   #:+severity-level-critical+
   #:+severity-level-error+
   #:+severity-level-warning+
   #:+severity-level-notice+
   #:+severity-level-info+
   #:+severity-level-debug+

   #:+severity-keyword-emergency+
   #:+severity-keyword-alert+
   #:+severity-keyword-critical+
   #:+severity-keyword-error+
   #:+severity-keyword-warning+
   #:+severity-keyword-notice+
   #:+severity-keyword-info+
   #:+severity-keyword-debug+

   #:*log-emergency-fn*
   #:*log-alert-fn*
   #:*log-critical-fn*
   #:*log-error-fn*
   #:*log-warn-fn*
   #:*log-notice-fn*
   #:*log-info-fn*
   #:*log-debug-fn*

   #:*default-nil-text*

   ;; FUNCTIONS & MACROS

   ;; Conditions & Error Handling

   #:severity-level-to-log-fn
   #:severity-keyword-to-severity-level
   #:severity-level-to-severity-keyword
   #:make-rds-info
   #:make-rds-info-string
   #:make-msg-info
   #:make-msg

   ;; Debugging

   #:maybe-invoke-debugger
   #:with-debugger
   #:ingore-errors*
   #:handler-case*
   #:get-backtrace

   ;; Text

   #:language-info
   #:make-language-desc
   #:make-text

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
