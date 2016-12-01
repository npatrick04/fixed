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
(defgeneric size (fp)
  (:documentation "Return the number of bits required by a ranged fp.  An fp type defined with zero or one limits returns :INFINITY."))

(defclass ordinary-fp (fp)
  ())

(defclass ranged-fp (fp)
  ())

(defclass decimal-fp (fp)
  ())

(defgeneric fixedp (object)
  (:documentation "Is the object an fixed point type.")
  (:method (object) nil)
  (:method ((object fp)) t))
(defgeneric ranged-fixedp (object)
  (:documentation "Is the object a ranged fixed point type.")
  (:method (object) nil)
  (:method ((object ranged-fp)) t))
(defgeneric decimal-fixedp (object)
  (:documentation "Is the object a decimal fixed point type.")
  (:method (object) nil)
  (:method ((object decimal-fp)) t))
(defgeneric ordinary-fixedp (object)
  (:documentation "Is the object an ordinary fixed point type.")
  (:method (object) nil)
  (:method ((object ordinary-fp)) t))

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

(defmacro math (mname name sv-name v-name fn)
  `(defmethod ,mname ((value ,name) &rest rest)
     (,sv-name (make-instance ',name)
	       (apply #',fn (,v-name value) (mapcar #',v-name rest)))))

(defparameter *rounding-method* #'round
  "#'round or #'truncate or similar functions will be used to handle precision loss.")

(defun pwr-of-2? (number)
  "Is a number a power of 2?  If so, return that power."
  (let ((pwr? (1- (integer-length number))))
    (when (and (not (minusp pwr?))
               (eq number (ash 1 pwr?)))
      pwr?)))

(defun q-class-name (pwr &optional bits)
  (intern (format nil "Q~:[~;~:*~D.~]~D" bits pwr) (find-package 'fixed)))

(defmacro make-q-class (pwr)
  `(progn
     (defclass ,(q-class-name pwr) (ordinary-fp)
       ())))

(defun symb (&rest strings)
  (intern (apply #'concatenate 'string strings)))

(defmacro %defdelta (name delta low high small super)
  "Do the work of creating the delta type."
  (let ((namestring (symbol-name name)))
    (let ((make-name          (symb "MAKE-" namestring))
          (make-raw-name      (symb "MAKE-" namestring "-VALUE"))
          (set-name           (symb "SET-" namestring))
          (set-raw-name       (symb "SET-" namestring "-VALUE"))
          (raw-name           (symb namestring "-VALUE"))
          (most-negative-name (symb "+MOST-NEGATIVE-" namestring "+"))
          (most-positive-name (symb "+MOST-POSITIVE-" namestring "+"))
          (small (if small
                     (eval small)
                     (expt 2 (- (ceiling (log (/ delta) 2)))))))

      ;; Ensure small is valid, which only matters when provided by the user.
      (unless (>= delta small)
        (error "Delta type created with SMALL ~A and DELTA ~A.
Delta types cannot be created with a SMALL that is larger than the DELTA."
               small
               delta))
      
      ;; Calculate the low and high count values
      ;; If ranges aren't provided, substitute with '* to support its
      ;; use in type specifiers.
      (let ((range-p    (or low high))
            (low-range  (if low  (ceiling (eval low)  small) '*))
            (high-range (if high (floor   (eval high) small) '*))
            (super-class (and (eq super 'ordinary-fp)
                              (pwr-of-2? (/ small))
                              (q-class-name (pwr-of-2? (/ small))))))
        `(progn
           ,(when super-class
              `(unless (find-class ',super-class nil)
                 (make-q-class ,(pwr-of-2? (/ small)))))
           (defclass ,name ,(cond
                              ((and range-p super-class) (list super-class super 'ranged-fp))
                              (super-class (list super-class super))
                              (range-p (list super 'ranged-fp))
                              (t `(,super)))
             ((value :reader ,raw-name
                     :writer (setf %value)
                     :initarg :value
                     :type ,(if range-p
                                `(integer ,low-range ,high-range)
                                `integer))))
           (defmethod small ((fp ,name))
             ,small)
           (defmethod small ((fp (eql ',name)))
             ,small)
           (defmethod delta ((fp ,name))
             ,delta)
           (defmethod delta ((fp (eql ',name)))
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

           ;; The setter when sharing a common super-class
           ,(when super-class
              `(defmethod ,set-name ((fp ,name) (value ,super-class))
                 (,set-raw-name fp (slot-value value 'value))))
           (defun ,make-name (value)
             (,set-name (make-instance ',name) value))
           (defun ,make-raw-name (value)
             (,set-raw-name (make-instance ',name) value))
           (defun ,name (fp)
             "Return the number."
             (* (the integer (,raw-name fp))
                ,small))

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

           ;; The constant things about this type
           (unless (and (boundp ',most-positive-name)
                        (or (symbolp ,most-positive-name)
                            (f= ,most-positive-name
                                (,make-raw-name ,high-range))))
             (defconstant ,most-positive-name
               ,(if high
                    `(,make-raw-name ,high-range)
                    :positive-infinity)))
           (unless (and (boundp ',most-negative-name)
                        (or (symbolp ,most-positive-name)
                            (f= ,most-negative-name
                                (,make-raw-name ,low-range))))
             (defconstant ,most-negative-name
               ,(if low
                    `(,make-raw-name ,low-range)
                    :negative-infinity)))

           (defmethod size ((fp ,name))
             ,(if (and high low)
                  (max (integer-length high-range)
                       (integer-length low-range))
                  :infinity))
           (defmethod size ((fp (eql ',name)))
             ,(if (and high low)
                  (max (integer-length high-range)
                       (integer-length low-range))
                  :infinity))
           
           ',name)))))

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
  `(prog1
     (%defdelta ,name ,delta ,low ,high ,small ordinary-fp)
     (defmethod ordinary-fixedp ((object (eql ',name))) t)))

(defmacro defdecimal (name digits &key low high)
  "A short-cut for defining a base-10 decimal type, that also happens
  to be printed correctly."
  (let ((delta (expt 10 (- (eval digits))))
	(raw-name (symb (symbol-name name) "-VALUE")))
    `(prog1
       (%defdelta ,name ,delta ,(eval low) ,(eval high) ,delta decimal-fp)
       (defmethod decimal-fixedp ((object (eql ',name))) t)
       (defmethod print-object ((object ,name) stream)
	 (print-unreadable-object (object stream :type t)
	   (multiple-value-bind (quotient remainder)
	       (truncate (,raw-name object) (/ ,delta))
	     (format stream "~D.~v,,,'0<~D~>"
		     quotient
		     ,digits 
		     (abs remainder))))))))
