;;;; fixed.lisp

(in-package #:fixed)

;;; All fixed-point types have values.
;;; This value will be shadowed by the specific definitions below.
(defclass fp ()
  ((value)))
(defgeneric small (fp)
  (:documentation "Return the scaling factor used by fp."))
(defgeneric delta (fp)
  (:documentation "Return the delta used by fp."))

(defclass ordinary-fp (fp)
  ())

(defclass ordinary-ranged-fp (fp)
  ())

;;; Rule of fixed contagion...it's not.
(defgeneric f= (number &rest rest)
  (:documentation "Determine if fixed-point values of the same
type are equal."))
(defgeneric f/= (number &rest rest)
  (:documentation "Determine if fixed-point values of the same
type are not equal."))
(defgeneric f> (number &rest rest)
  (:documentation "Determine if fixed-point values of the same
type are in descending order."))
(defgeneric f>= (number &rest rest)
  (:documentation "Determine if fixed-point values of the same
type are in descending-or-equal order."))
(defgeneric f< (number &rest rest)
  (:documentation "Determine if fixed-point values of the same
type are in ascending order."))
(defgeneric f<= (number &rest rest)
  (:documentation "Determine if fixed-point values of the same
type are in ascending-or-equal order."))

(defmacro predicate (pname name raw fn)
  `(defmethod ,pname ((number ,name) &rest rest)
     (if rest
	 (let ((next (the ,name (car rest))))
	   (and (,fn (,raw number)
		     (,raw next))
		(apply #',pname next (cdr rest))))
	 t)))

(defgeneric f+ (value &rest rest)
  (:documentation "Sum all fixed-point arguments of a single type."))
(defgeneric f- (value &rest rest)
  (:documentation "Subtract all fixed-point arguments of a single type
  from the first value when more than one value are provided.  If only
  a single value is provided, negate that value."))
(defgeneric f* (value &rest rest)
  (:documentation "Multiply all fixed-point arguments of a single
  type.  The first value returned is the resultant fixed-point value,
  the second value is the residual."))
(defgeneric f/ (value &rest rest)
  (:documentation "Divide the first value by the rest, or perform the
  inverse operation if a single value is provided.
  The first value returned is the resultant fixed-point value, the
  second value is the residual."))
;;; TODO f/
(defmacro math (mname name sv-name v-name fn)
  `(defmethod ,mname ((value ,name) &rest rest)
     (,sv-name (make-instance ',name)
	       (apply #',fn (,v-name value) (mapcar #',v-name rest)))))

(defparameter *rounding-method* #'round
  "#'round or #'truncate or similar functions will be used to handle precision loss.")

(defmacro defdelta (name delta &key low high small)
  "Define a fixed point class named NAME which supports a resolution
  of DELTA.  The class maintains the value as an
  (INTEGER LOW-RANGE HIGH-RANGE) where the LOW-RANGE and HIGH_RANGE are
  determined to provide an engineering-unit value within the range of low
  to high.

  When SMALL is provided, the resolution of the type is defined to
  be exactly SMALL.  When only DELTA is provided, the resolution of
  the type is defined to be the negative power of 2 that is no larger
  than DELTA.

  e.g. (defdelta foo 1/10) yields a SMALL value of 1/16.
       (defdelta foo 1/10 :small 1/10) yields a SMALL value of 1/10.

  This definition also produces a set of related functions and generic
  methods for working with the NAME type.
- MAKE-NAME: Creates a NAME type with initial value rounded to the
  provided value. 
- MAKE-NAME-VALUE: Creates a NAME type with internal value as
  provided. 
- SET-NAME: A function to set the internal value according to the
  engineering unit value provided by rounding.
- SET-NAME-VALUE: A function to set the internal value.
- NAME-VALUE: Accessor (setf'able) for the internal value.
- NAME: Accessor (setf'able) for the engineering unit value.

- Predicates: f= f/= f< f<= f> f>=
- Math operations: f+ f- f* f/"
  (let ((make-name (intern (concatenate 'string
					"MAKE-"
					(symbol-name name))))
	(make-raw-name (intern (concatenate 'string
					    "MAKE-"
					    (symbol-name name)
					    "-VALUE")))
	(set-name (intern (concatenate 'string
				       "SET-"
				       (symbol-name name))))
	(set-raw-name (intern (concatenate 'string
					   "SET-"
					   (symbol-name name)
                                           "-VALUE")))
	(raw-name (intern (concatenate 'string
				       (symbol-name name)
				       "-VALUE")))
        (small (if small small
		   (expt 2 (- (ceiling (log (/ delta) 2)))))))
    ;; Calculate the low and high count values
    ;; If ranges aren't provided, substitute with '* to support its
    ;; use in type specifiers.
    (let ((range-p    (or low high))
          (low-range  (if low  (ceiling low  small) '*))
          (high-range (if high (floor   high small) '*)))
      `(progn
         (defclass ,name (,(if range-p
                               'ordinary-ranged-fp
                               'ordinary-fp))
           ((value :reader ,raw-name
                   :writer (setf %value)
                   :initarg :value
                   :type ,(if range-p
                              `(integer ,low-range ,high-range)
                              `integer))))
	 (defmethod small ((fp ,name))
	   ,small)
	 (defmethod delta ((fp ,name))
	   ,delta)
         (defun ,set-raw-name (fp value)
	   (declare (type ,name fp)
		    (type integer value))
           ,(if range-p
                `(setf (%value fp)
                       (the (integer ,low-range ,high-range)
                            value))
                `(setf (%value fp)
                       (the integer value)))
           fp)
	 (defgeneric ,set-name (fp value)
	   (:documentation "Set the fp to some value"))
	 (defmethod ,set-name ((fp ,name) (value real))
	   (multiple-value-bind
		 (quotient remainder) (funcall *rounding-method* value ,small)
	     (values (,set-raw-name fp quotient)
		     remainder)))
	 (defmethod ,set-name ((fp ,name) (value ,name))
	   ;; Just set it equal and return the fp.
	   (setf (%value fp) (,raw-name value))
	   (values fp 0))
	 (defmethod ,set-name ((fp ,name) (value fp))
	   ;; Now it's interesting, another fp type, but not name.
	   (let ((conversion-ratio (/ (small value) (small fp))))
	     (multiple-value-bind
		   (quotient remainder)
		 (funcall *rounding-method*
			  (* conversion-ratio
			     (slot-value value 'value)))
	       (values (,set-raw-name fp quotient)
		       remainder))))
         (defun ,make-name (value)
           (,set-name (make-instance ',name) value))
         (defun ,make-raw-name (value)
           (,set-raw-name (make-instance ',name) value))
         (defun ,name (fp)
           ;; There's got to be a better way...
           (declare (optimize speed))
           (* ,delta
              (funcall *rounding-method* (* (the integer (,raw-name fp))
				 ,small)
		       ,delta)))

         (defmethod print-object ((object ,name) stream)
           (print-unreadable-object (object stream :type t)
             (format stream "~A" (,name object))))

         (defsetf ,name ,set-name)
         (defsetf ,raw-name ,set-raw-name)

         (predicate f=  ,name ,raw-name =)
         (predicate f/= ,name ,raw-name /=)
         (predicate f>  ,name ,raw-name >)
         (predicate f>= ,name ,raw-name >=)
         (predicate f<  ,name ,raw-name <)
         (predicate f<= ,name ,raw-name <=)

         (math f+ ,name ,set-raw-name ,raw-name +)
         (math f- ,name ,set-raw-name ,raw-name -)
         ;; * and / are not as simple
         ;; v = i1 * small
         ;; v1 * v2 == i1 * small * i2 * small
         (defmethod f* ((value ,name) &rest rest)
           (multiple-value-bind (quotient remainder)
               (funcall *rounding-method* (* (apply #'*	; Product of the integral parts
						    (,raw-name value)
						    (mapcar #',raw-name rest))
					     ;; multiplied by small for each rest parameter
					     (expt ,small (length rest))))
             (values (,set-raw-name (make-instance ',name)
                                    quotient)
                     (* remainder ,small))))
         ;; v = i * small
         ;; (/ v1 v2) == (* v1 small) / (* v2 small)
         ;; (/ v1 v2 v3) == (* v1 small) / (* v2 v3 small small)
         (defmethod f/ ((value ,name) &rest rest)
           (multiple-value-bind (quotient remainder)
               (if rest
		   (funcall *rounding-method*
			    (/ (apply #'/ (,raw-name value)
				      (mapcar #',raw-name rest))
			       (expt ,small (length rest))))
		   (funcall *rounding-method*
			    (/ 1 (,raw-name value) ,small) ,small))
             (values (,set-raw-name (make-instance ',name)
                                    quotient)
                     (* remainder ,small))))

         ',name))))

(defmacro defdecimal (name digits &key low high)
  "A short-cut for defining a base-10 decimal type, that also happens
  to be printed correctly."
  (let ((delta (gensym "DELTA"))
	(raw-name (intern (concatenate 'string
				       (symbol-name name)
				       "-VALUE"))))
    `(let ((,delta (expt 10 (- ,digits))))
       (declare (type rational ,delta))
       (defdelta ,name ,delta :low ,low :high ,high :small ,delta)
       (defmethod print-object ((object ,name) stream)
	 (print-unreadable-object (object stream :type t)
	   (multiple-value-bind (quotient remainder)
	       (truncate (,raw-name object) (/ ,delta))
	     (format stream "~D.~v,,,'0<~D~>"
		     quotient
		     ,digits 
		     (abs remainder))))))))
