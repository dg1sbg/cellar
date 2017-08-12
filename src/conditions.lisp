;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;; ==============================================================

(in-package #:net.goenninger.lib.cellar)

;;; --------------------------------------------------------------
;;; SPECIAL VARS
;;; --------------------------------------------------------------

(defv *catch-errors-p*      nil)
(defv *invoke-debugger-p*   nil)
(defv *print-backtrace-p*   nil)
(defv *log-using-log4cl-p*    t)

(defc +severity-level-emergency+ 0)
(defc +severity-level-alert+     1)
(defc +severity-level-critical+  2)
(defc +severity-level-error+     3)
(defc +severity-level-warning+   4)
(defc +severity-level-notice+    5)
(defc +severity-level-info+      6)
(defc +severity-level-debug+     7)

(defc +severity-keyword-emergency+ :emergency)
(defc +severity-keyword-alert+     :alert)
(defc +severity-keyword-critical+  :critical)
(defc +severity-keyword-error+     :error)
(defc +severity-keyword-warning+   :warning)
(defc +severity-keyword-notice+    :notice)
(defc +severity-keyword-info+      :info)
(defc +severity-keyword-debug+     :debug)

;;; NOTES ON THE RDP-PP RELATED DESIGNATOR SYSTEM WE USE
;;;
;;; The RDS-PP is described here:
;;; https://www.vgb.org/en/db_rds_e.html

(defstruct rds-info
  ;; Example: =G01 LAC21 BC010 -MA01
  (plant-function-designator    "" :type string) ;; Level 0, =ANN
  (main-function-designator     "" :type string) ;; Level 1, AAANN
  (sub-function-designator      "" :type string) ;; Level 2, AANNN
  (product-reference-designator "" :type string) ;; Level 3, -AANN
  )

(deftype category-nr-type ()
  '(integer 0 *))

(defstruct rds-object-info
  ;; 1 = Piece Part
  ;; 2 =
  (category-nr    0  :type category-nr-type)
  (category-title "" :type string))

(defstruct msg-info
  (category-nr      0          :type category-nr-type)
  (category-title   ""         :type string)
  (category-keyword :undefined :type symbol)
  )

(defun make-rds-info-string (&key
			       plant-function-designator$
			       main-function-designator$
			       sub-function-designator$
			       product-reference-designator$)
  (with-output-to-string (s string)
    (format s "~4a ~5a ~5a ~5a"
	    (or plant-function-designator$ "")
	    (or main-function-designator$ "")
	    (or sub-function-designator$ "")
	    (or product-reference-designator$ ""))
    string))

(defstruct msg
  (nr    -1 :type integer)
  (text  cellar:*default-nil-text* :type cellar:text))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun default-log-emergency (&rest args)
    (log:fatal args))

  (defun default-log-alert (&rest args)
    (log:fatal args))

  (defun default-log-critical (&rest args)
    (log:fatal args))

  (defun default-log-error (&rest args)
    (log:error args))

  (defun default-log-warn (&rest args)
    (log:warn args))

  (defun default-log-notice (&rest args)
    (log:info args))

  (defun default-log-info (&rest args)
    (log:info args))

  (defun default-log-debug (&rest args)
    (log:debug args))
  )

(defv *log-emergency-fn*  #'default-log-emergency)
(defv *log-alert-fn*      #'default-log-alert)
(defv *log-critical-fn*   #'default-log-critical)
(defv *log-error-fn*      #'default-log-error)
(defv *log-warn-fn*       #'default-log-warn)
(defv *log-notice-fn*     #'default-log-notice)
(defv *log-info-fn*       #'default-log-info)
(defv *log-debug-fn*      #'default-log-debug)

(defv *severity-level-to-log-fn-ht* (make-hash-table))

;;; --------------------------------------------------------------
;;; CONDITION DEFINITIONS
;;; --------------------------------------------------------------

(defun set-severity-level-log-fn (severity-level log-fn)
  (setf (gethash severity-level *severity-level-to-log-fn-ht*) log-fn))

(defun init-severity-level-to-log-fn-ht ()
  (set-severity-level-log-fn +severity-level-emergency+ *log-emergency-fn*)
  (set-severity-level-log-fn +severity-level-alert+     *log-alert-fn*)
  (set-severity-level-log-fn +severity-level-critical+  *log-critical-fn*)
  (set-severity-level-log-fn +severity-level-error+     *log-error-fn*)
  (set-severity-level-log-fn +severity-level-warn+      *log-warn-fn*)
  (set-severity-level-log-fn +severity-level-notice+    *log-notice-fn*)
  (set-severity-level-log-fn +severity-level-info+      *log-info-fn*)
  (set-severity-level-log-fn +severity-level-debug+     *log-debug-fn*)
  *severity-level-to-log-fn-ht*)

(init-severity-level-to-log-fn-ht)

(defun severity-level-to-log-fn (severity-level)
  (gethash severity-level *severity-level-to-log-fn-ht*))

(defun severity-keyword-to-severity-level (severity-keyword)
  (case severity-keyword
    (+severity-keyword-emergency+  +severity-level-emergency+)
    (+severity-keyword-alert+      +severity-level-alert+)
    (+severity-keyword-critical+   +severity-level-critical+)
    (+severity-keyword-error+      +severity-level-error+)
    (+severity-keyword-warning+    +severity-level-warning+)
    (+severity-keyword-notice+     +severity-level-notice+)
    (+severity-keyword-info+       +severity-level-info+)
    (+severity-keyword-debug+      +severity-level-debug+)
    (t                             +severity-level-debug+)
    ))

(defun severity-level-to-severity-keyword (severity-level)
  (case severity-level
    (+severity-level-emergency+  +severity-keyword-emergency+)
    (+severity-level-alert+      +severity-keyword-alert+)
    (+severity-level-critical+   +severity-keyword-critical+)
    (+severity-level-error+      +severity-keyword-error+)
    (+severity-level-warning+    +severity-keyword-warning+)
    (+severity-level-notice+     +severity-keyword-notice+)
    (+severity-level-info+       +severity-keyword-info+)
    (+severity-level-debug+      +severity-keyword-debug+)
    (t                           :undefined)
    ))

;;; Report function used by all conditions

(defun report-condition (condition &optional stream)

  (check-type condition condition)

  (with-output-to-string (s string)

    (let* ((severity-level (category condition))
	   (log-fn (severity-level-to-log-fn sevrity-level))
	   (severity-label (symbol-name (severity-level-to-severity-keyword severity-level)))
	   (format-control   (format-control   condition))
	   (format-arguments (format-arguments condition))
	   (format-arguments (if (listp format-arguments)
				 (car format-arguments)
				 format-arguments))
	   (condition-category (category condition)))
      (format s "~a @ ~a ~d => ~&"
	      severity-label
	      (src      condition)
	      (category condition)
	      (nr       condition))
      (format s "~&*** ")

      (apply #'format s
	     format-control
	     format-arguments)
      (format s "~%")

      (when (and *print-backtrace-p*
		 (eql condition-category :CRITICAL))
	(format s "~&*** Backtrace:~&")
	(format s "~&~A~%" (get-backtrace)))

      (when stream
	(format stream "~s" string))

      (when *log-using-log4cl-p*
	(let ((log-fn (severity-level-to-log-fn condition-category)))
	  (apply log-fn string)))
      )))

;;; BASE CONDITION

(define-condition cellar-condition (condition)
  ((category         :initarg :category         :accessor category :initform nil)
   (src              :initarg :src              :accessor src      :initform nil)
   (nr               :initarg :nr               :accessor nr       :initform 0)
   (format-control   :initarg :format-control   :accessor format-control
                     :initform nil)
   (format-arguments :initarg :format-arguments :accessor format-arguments
                     :initform nil))
  (:report (lambda (condition stream)
             (report-condition condition stream)))
  (:documentation "Superclass for all conditions."))

;;; DEBUG CONDITION

(define-condition debug-condition (cellar-condition)
  ()
  (:default-initargs
   :category :debug)
  (:documentation "Superclass for all debug messages."))

(define-condition simple-debug-condition (debug-condition simple-condition)
  ()
  (:documentation "Like DEBUG but with formatting capabilities."))

(defun signal-debug-condition (src nr format-control &rest format-arguments)
  "Signals a condition of type CCAG-SIMPLE-DEBUG with the provided
format control and arguments."
  (signal 'simple-debug-condition
          :src src
          :nr nr
          :format-control format-control
          :format-arguments format-arguments))

;;; INFO CONDITION

(define-condition info-condition (cellar-condition)
  ()
  (:default-initargs
   :category :info)
  (:documentation "Superclass for all info messages."))

(define-condition simple-info-condition (info-condition simple-condition)
  ()
  (:documentation "Like INFO but with formatting capabilities."))

(defun signal-info-condition (src nr format-control &rest format-arguments)
  "Signals a condition of type SIMPLE-DEBUG with the provided
format control and arguments."
  (signal 'simple-info-condition
          :src src
          :nr nr
          :format-control format-control
          :format-arguments format-arguments))

;;; NOTICE CONDITION

(define-condition notice-condition (cellar-condition)
  ()
  (:default-initargs
   :category :notice)
  (:documentation "Superclass for all Notice messages."))

(define-condition simple-notice-condition (notice-condition simple-condition)
  ()
  (:documentation "Like NOTICE but with formatting capabilities."))

(defun signal-notice-condition (src nr format-control &rest format-arguments)
  "Signals a condition of type CCAG-SIMPLE-NOTICE with the provided
format control and arguments."
  (signal 'simple-notice-condition
          :src src
          :nr nr
          :format-control format-control
          :format-arguments format-arguments))

;;; WARNING CONDITION

(define-condition warning-condition (cellar-condition)
  ()
  (:documentation "Superclass for all Notice messages."))

(define-condition simple-warning-condition (notice-condition simple-condition)
  ()
  (:default-initargs
   :category :warning)
  (:documentation "Like WARNING but with formatting capabilities."))

(defun signal-warning-condition (src nr format-control &rest format-arguments)
  "Signals a condition of type SIMPLE-WARNING with the provided
format control and arguments."
  (signal 'simple-warning-condition
          :src src
          :nr nr
          :format-control format-control
          :format-arguments format-arguments))

;;; ERROR CONDITION

(define-condition error-condition (cellar-condition error)
  ()
  (::default-initargs
   :category :error)
  (:documentation "Superclass for all errors."))

(define-condition simple-error-condition (error-condition simple-condition)
  ()
  (:documentation "Like ERROR but with formatting capabilities."))

(defun signal-error-condition (src nr format-control &rest format-arguments)
  "Signals an error of type SIMPLE-ERROR with the provided
format control and arguments."
  (error 'simple-error-condition
	 :src src
	 :nr nr
	 :format-control format-control
	 :format-arguments format-arguments))

;;; CRITICAL CONDITION

(define-condition critical-condition (cellar-condition error)
  ()
  (:default-initargs
   :category :critical)
  (:documentation "Superclass for all critical errors."))

(define-condition simple-critical-condition (critical-condition simple-condition)
  ()
  (:documentation "Like CRITICAL but with formatting capabilities."))

(defun signal-critical-condition (src nr format-control &rest format-arguments)
  "Signals an error of type SIMPLE-CRITICAL with the provided
format control and arguments."
  (error 'simple-critical-condition
	 :src src
	 :nr nr
	 :format-control format-control
	 :format-arguments format-arguments))

;;; ALERT CONDITION

(define-condition alert-condition (cellar-condition error)
  ()
  (:default-initargs
   :category :alert)
  (:documentation "Superclass for all alerts."))

(define-condition simple-alert-condition (alert-condition simple-condition)
  ()
  (:documentation "Like WARNING-CONDITION but with formatting capabilities."))

(defun signal-alert-condition (src nr format-control &rest format-arguments)
  "Signals a warning of type SIMPLE-ALERT with the provided format control and arguments."
  (error 'simple-alert-condition
	 :src src
	 :nr nr
	 :format-control format-control
	 :format-arguments format-arguments))

;;; EMERGENCY CONDITION

(define-condition emegrency-condition (cellar-condition error)
  ()
  (:default-initargs
   :category :emergency)
  (:documentation "Superclass for all emergencies."))

(define-condition simple-emergency-condition (emergency-condition simple-condition)
  ()
  (:documentation "Like EMERGENCY-CONDITION but with formatting capabilities."))

(defun signal-emergemcy-condition (src nr format-control &rest format-arguments)
  "Signals a warning of type SIMPLE-EMERGENCY with the provided format control and arguments."
  (error 'simple-emergency-condition
	 :src src
	 :nr nr
	 :format-control format-control
	 :format-arguments format-arguments))

;;; --------------------------------------------------------------
;;; DEBUGGER HANDLING
;;; --------------------------------------------------------------

(defgeneric maybe-invoke-debugger (condition)
  (:documentation "This generic function is called whenever a
condition CONDITION is signaled.  You might want to
specialize it on specific condition classes for debugging purposes.")
  (:method (condition)
   "The default method invokes the debugger with CONDITION if
*CATCH-ERRORS-P* is NIL and *INVOKE-DEBUGGER-P* is T."
           (when (and (not *catch-errors-p*)
                      *invoke-debugger-p*)
             (invoke-debugger condition))))

(defmacro with-debugger (&body body)
  "Executes BODY and invokes the debugger if an error is signaled and
*CATCH-ERRORS-P* is NIL and INVOKE-DEBUGGER-P* is T."
  `(handler-bind ((error #'maybe-invoke-debugger))
     ,@body))

(defmacro ignore-errors* (&body body)
  "Like IGNORE-ERRORS, but observes *CATCH-ERRORS-P* and *INVOKE-DEBUGGER-P*."
  `(ignore-errors (with-debugger ,@body)))

(defmacro handler-case* (expression &rest clauses)
  "Like HANDLER-CASE, but observes *CATCH-ERRORS-P* and *INVOKE-DEBUGGER-P*."
  `(handler-case (with-debugger ,expression)
     ,@clauses))

(defun get-backtrace ()
  "Returns a string with a backtrace of what the Lisp system thinks is
the \"current\" error."
  (handler-case
      (with-output-to-string (s)
        (trivial-backtrace:print-backtrace-to-stream s))
    (error (condition)
      (format nil "Could not generate backtrace: ~A." condition))))
