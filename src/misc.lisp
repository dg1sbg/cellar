;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-

(in-package #:net.goenninger.lib.cellar)

(defun intern-with-package$ (package &rest strings)
  (intern (apply #'concatenate 'string strings) package))

(defun intern-as-keyword$ (&rest strings)
  (intern (apply #'concatenate 'string strings) (find-package "KEYWORD")))

(defun stringify (something)
  (typecase something
    (string (string-upcase something))
    (symbol (symbol-name something))
    (t (with-output-to-string (string)
	 (format string "~S" something)))))

(defun getenv (var)
  "Return the value of the environment variable."
  (uiop:getenv var))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro export! (symbol)
    `(eval-when (:load-toplevel :compile-toplevel :execute)
       (export ,symbol))))

(defun argv0 ()
  #+allegro (sys:command-line-argument 0)
  #+lispworks (nth 0 system:*line-arguments-list*) ;; portable to OS X
  #+sbcl (nth 0 sb-ext:*posix-argv*)
  #+openmcl (car ccl:*command-line-argument-list*)
  #-(or allegro lispworks sbcl openmcl)
  (error "argv0 function not implemented for this lisp"))

(defun string-join (strings &optional (delim ""))
  "Join a list of elements into a string and place delim between the elements then return the string"
  (with-output-to-string (out)
    (loop for (string . more?) on strings
       do (write-string string out)
       when more? do (write-string delim out))))

(defun alternate-functions (&rest functions)
  "return a function that calls the next function of the FUNCTIONS list each time it's called, in a circular way"
  (let* ((funs (copy-list functions))   ; don't scribble on &rest args
	 (funs (nconc funs funs)))      ; build a circular list
    (lambda (&rest x)
      (prog1 (apply (car funs) x)
	(setf funs (cdr funs))))))

;;; CELLS UTILS

#+cells
(defmacro defdbgobserver (slot-name (class-name))
  `(cells:defobserver ,slot-name ((self ,class-name))
     (when (not (eql new-value old-value))
       #+log4cl
       (log:debug "~S: Slot ~A: Old value: ~S => New value: ~S"
		  self ',slot-name old-value new-value))))

;;; CONFIG VAR

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-config-var-value (name-symbol default-value)
    (let ((envvar-value (cellar:getenv (symbol-name name-symbol))))
      #+log4cl
      (log:debug "ENV: ~A = ~S. Will return value ~S !" name-symbol envvar-value default-value)
      (or envvar-value default-value)))

  (defun generate-define-config-var-code (name value)
    (let ((cname (intern (concatenate
			  'string "+" (symbol-name name) "+")
			 (symbol-package name)))
	  (vname (intern (concatenate
			  'string "*" (symbol-name name) "*")
			 (symbol-package name)))
	  (setter-name (intern (concatenate
				'string "*" (symbol-name name) "*!")
			       (symbol-package name))))
      `(eval-when (:compile-toplevel :load-toplevel)
	 (defc ,cname ,value)
	 (defv ,vname (get-config-var-value ',name ,cname))
	 (defun ,setter-name (value)
	   (setf ,vname value))
	 (export '(,cname ,vname ,setter-name))
	 )))

  (defmacro define-config-var (name value)
    (generate-define-config-var-code name value)))

;; stolen from metatilities

(defmacro muffle-redefinition-warnings (&body body)
  "Evaluate the body so that redefinition warnings will not be
signaled. (suppored in Allegro, Clozure CL, CLisp, and Lispworks)"
  #+allegro
  `(excl:without-redefinition-warnings
     ,@body)
  #+(or ccl mcl)
  `(let ((ccl::*warn-if-redefine* nil)
	 ;;?? FIXME not sure if this should be here or not...
	 (ccl::*record-source-file* nil))
     ,@body)
  #+clisp
  `(let ((custom:*suppress-check-redefinition* t))
     ,@body)
  #+lispworks
  `(let ((lw:*handle-warn-on-redefinition* :quiet))
     ,@body)
  #+sbcl
  ;; from http://www.sbcl.info/manual/Controlling-Verbosity.html
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note
					       sb-ext::style-warning))
     ,@body)
  #-(or allegro ccl clisp mcl sbcl)
  `(progn ,@body))
