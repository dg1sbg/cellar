(in-package #:net.goenninger.lib.cellar)

(defvar *in-progress-objects* nil)

(defc +null+ (code-char 0))

(defgeneric read-value-of-type (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value-of-type (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defmethod read-value-of-type ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value-of-type ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))

;;; Binary types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-reader-body (spec stream)
    (ecase (length spec)
      (1 (destructuring-bind (type &rest args) (mklist (first spec))
           `(read-value-of-type ',type ,stream ,@args)))
      (2 (destructuring-bind ((in) &body body) (cdr (assoc :reader spec))
           `(let ((,in ,stream)) ,@body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-writer-body (spec stream value)
    (ecase (length spec)
      (1 (destructuring-bind (type &rest args) (mklist (first spec))
           `(write-value-of-type ',type ,stream ,value ,@args)))
      (2 (destructuring-bind ((out v) &body body) (cdr (assoc :writer spec))
           `(let ((,out ,stream) (,v ,value)) ,@body))))))

(defmacro define-binary-type (name (&rest args) &body spec)
  (utils-kt::with-gensyms (type stream value)
  `(progn
    (defmethod read-value-of-type ((,type (eql ',name)) ,stream &key ,@args)
      (declare (ignorable ,@args))
      ,(type-reader-body spec stream))
    (defmethod write-value-of-type ((,type (eql ',name)) ,stream ,value &key ,@args)
      (declare (ignorable ,@args))
      ,(type-writer-body spec stream value))
    (export ',name))))

;;; Binary classes

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       ,read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots)))

       (export ',name))))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
      (defmethod read-value-of-type ((,typevar (eql ',name)) ,streamvar &key)
        (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
          (let ((,objectvar
                 (make-instance
                  ,@(or (cdr (assoc :dispatch options))
                        (error "Must supply :disptach form."))
                  ,@(mapcan #'slot->keyword-arg slots))))
            (read-object ,objectvar ,streamvar)
            ,objectvar))))))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value-of-type ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value-of-type ',type ,stream ,name ,@args)))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value-of-type ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

;;; Keeping track of inherited slots

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  "Like all slots but works while compiling a new class before slots
and superclasses have been saved."
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

;;; In progress Object stack

(defun current-binary-object ()
  (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

;; support for big/little endian handling
(eval-when (:compile-toplevel :load-toplevel :execute)

  (deftype endianess ()
    "Defines the legal values for *endianess*."
    '(member :big-endian :little-endian))

  (defvar *endianess* nil
    "*endianess* has to be either :big-endian or :little-endian")

  (defun define-endianess (endian)
    (check-type endian endianess)
    (setf *endianess* endian))

#+little-endian (define-endianess :little-endian)
#+big-endian (define-endianess :big-endian)

#+(not (or little-endian big-endian))
  (if (not *endianess*)
    (error "Cannot detect endianness ! Please set via (define-endianess ...)"))
)

(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
   (ecase *endianess*
     ((:big-endian)
        (loop with value = 0
         for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
           (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
         finally (return value)))
     ((:little-endian)
        (loop with value = 0
         for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte do
           (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
         finally (return value)))))
  (:writer (out value)
   (ecase *endianess*
     ((:little-endian)
        (loop for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
           do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))
     ((:big-endian)
        (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
           do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

(define-binary-type ul () (unsigned-integer :bytes 4 :bits-per-byte 8))

(define-binary-type IEEE-float (sign-bits exponent-bits mantissa-bits bias associated-type)
  (:reader (in)
    (let* ((value (read-value-of-type 'u4 in))
           (total-bits (+ 1 exponent-bits mantissa-bits))
	   (exponent-offset (1- (expt 2 (1- exponent-bits))))
	   (sign-part (ldb (byte 1 (1- total-bits)) value))
	   (exponent-part (ldb (byte exponent-bits mantissa-bits) value))
	   (significand-part (ldb (byte mantissa-bits 0) value))
           (sign sign-part)
           (exponent exponent-part)
	   (significand significand-part))
        (if (zerop exponent)
            (setf exponent 1)
            (setf (ldb (byte 1 mantissa-bits) significand) 1))
	(unless (zerop sign)
	  (setf significand (- significand)))
	(scale-float (float significand (if (> total-bits 32) 1.0d0 1.0))
	 	     (- exponent (+ exponent-offset mantissa-bits)))))
  (:writer (out value)
    (let* ((total-bits (+ 1 exponent-bits mantissa-bits))
	   (exponent-offset (1- (expt 2 (1- exponent-bits))))
	   (sign-part (ldb (byte 1 (1- total-bits)) value))
	   (exponent-part (ldb (byte exponent-bits mantissa-bits) value))
	   (significand-part (ldb (byte mantissa-bits 0) value))
           (sign sign-part)
           (exponent exponent-part)
	   (significand significand-part))
     (write-value-of-type 'u4 out
      (multiple-value-bind (sign significand exponent)
       (cond ((zerop value) (values 0 0 0))
              (t (multiple-value-bind (significand exponent sign)
                 (decode-float value)
               (let ((exponent (+ (1- exponent) exponent-offset))
                     (sign (if (= sign 1.0) 0 1)))
                 (unless (< exponent (expt 2 exponent-bits))
                   (error "Floating point overflow when encoding ~A." value))
                 (if (< exponent 0)
                     (values sign (ash (round (* (expt 2 mantissa-bits) significand)) exponent) 0)
                     (values sign (round (* (expt 2 mantissa-bits) (1- (* significand 2)))) exponent))))))
       (let ((value 0))
         (setf sign-part sign
	       exponent-part exponent
	       significand-part significand)
         value)))))
  )

(define-binary-type IEEE-double-float ()
  (IEEE-float :sign-bits 1 :exponent-bits 11 :mantissa-bits 52 :bias 1023 :associated-type 'double-float))

(define-binary-type IEEE-single-float ()
  (IEEE-float :sign-bits 1 :exponent-bits  8 :mantissa-bits 23 :bias  127 :associated-type 'single-float))



;;; Strings

(define-binary-type generic-string (length character-type)
  (:reader (in)
    (let ((string (make-string length)))
      (dotimes (i length)
        (setf (char string i) (read-value-of-type character-type in)))
      string))
  (:writer (out string)
    (dotimes (i length)
      (write-value-of-type character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
    (with-output-to-string (s)
      (loop for char = (read-value-of-type character-type in)
            until (char= char terminator) do (write-char char s))))
  (:writer (out string)
    (loop for char across string
          do (write-value-of-type character-type out char)
          finally (write-value-of-type character-type out terminator))))

;;; ISO-8859-1 strings

(define-binary-type iso-8859-1-char ()
  (:reader (in)
    (let ((code (read-byte in)))
      (or (code-char code)
          (error "Character code ~d not supported" code))))
  (:writer (out char)
    (let ((code (char-code char)))
      (if (<= 0 code #xff)
          (write-byte code out)
          (error "Illegal character for iso-8859-1 encoding: character: ~c with code: ~d" char code)))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))

;;; UCS-2 (Unicode) strings (i.e. UTF-16 without surrogate pairs, phew.)

;;; Define a binary type for reading a UCS-2 character relative to a
;;; particular byte ordering as indicated by the BOM value.
 ;; v2.3 specifies that the BOM should be present. v2.2 is silent
 ;; though it is arguably inherent in the definition of UCS-2) Length
 ;; is in bytes. On the write side, since we don't have any way of
 ;; knowing what BOM was used to read the string we just pick one.
 ;; This does mean roundtrip transparency could be broken.

(define-binary-type ucs-2-char (swap)
  (:reader (in)
    (let ((code (read-value-of-type 'u2 in)))
      (when swap (setf code (swap-bytes code)))
      (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
    (let ((code (char-code char)))
      (unless (<= 0 code #xffff)
        (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d" char code))
      (when swap (setf code (swap-bytes code)))
      (write-value-of-type 'u2 out code))))

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
  code)

(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

(define-binary-type ucs-2-string (length)
  (:reader (in)
    (let ((byte-order-mark (read-value-of-type 'u2 in))
          (characters (1- (/ length 2))))
      (read-value-of-type
       'generic-string in
       :length characters
       :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
    (write-value-of-type 'u2 out #xfeff)
    (write-value-of-type
     'generic-string out string
     :length (length string)
     :character-type (ucs-2-char-type #xfeff))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
    (let ((byte-order-mark (read-value-of-type 'u2 in)))
      (read-value-of-type
       'generic-terminated-string in
       :terminator terminator
       :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
    (write-value-of-type 'u2 out #xfeff)
    (write-value-of-type
     'generic-terminated-string out string
     :terminator terminator
     :character-type (ucs-2-char-type #xfeff))))
