;;;; fixed.lisp

(in-package #:fixed)

(defclass fp ()
  ((value)))

(defclass ordinary-fp (fp)
  ())

(defclass ordinary-ranged-fp (fp)
  ())

;;; Rule of fixed contagion...it's not.
;; (declaim (ftype (function (fp &rest rest) (or t nil)))
;; 	 f= f/= f> f>= f< f<=)
(defgeneric f= (number &rest rest))
(defgeneric f/= (number &rest rest))
(defgeneric f> (number &rest rest))
(defgeneric f>= (number &rest rest))
(defgeneric f< (number &rest rest))
(defgeneric f<= (number &rest rest))

(defmacro predicate (pname name raw fn)
  `(defmethod ,pname ((number ,name) &rest rest)
     (if rest
	 (let ((next (the ,name (car rest))))
	   (and (,fn (,raw number)
		     (,raw next))
		(apply #',pname next (cdr rest))))
	 t)))

(defgeneric f+ (value &rest rest))
(defmacro math (mname name sv-name v-name fn)
  `(defmethod ,mname ((value ,name) &rest rest)
     (,sv-name (make-instance ',name)
	       (apply #',fn (,v-name value) (mapcar #',v-name rest)))))

(defmacro defdelta (name delta &key range small)
  (let ((fn-name (intern (concatenate 'string
                                      "MAKE-"
                                      (symbol-name name))))
	(set-name (intern (concatenate 'string
				       "SET-"
				       (symbol-name name))))
	(set-raw-name (intern (concatenate 'string
					   "SET-VALUE-"
					   (symbol-name name))))
	(raw-name (intern (concatenate 'string
				       (symbol-name name)
				       "-VALUE")))
        (small (if small small
		   (expt 2 (- (ceiling (log (/ delta) 2)))))))
    (let ((low  (and range (round (caadr range)  small)))
	  (high (and range (round (cadadr range) small))))
      `(progn
	 (defclass ,name (,(if range
			       'ordinary-ranged-fp
			       'ordinary-fp))
	   ((value :reader ,raw-name
		   :writer (setf %value)
		   :initarg :value
		   :type ,(if range
			      `(integer ,low ,high)
			      `integer))))
	 (defun ,set-raw-name (fp value)
	   ,(if range
		`(setf (%value fp)
		       (the (integer ,low ,high)
			    value))
		`(setf (%value fp)
		       (the integer value)))
	   fp)
	 (defun ,set-name (fp value)
	   (,set-raw-name fp (round value ,small)))
	 (defun ,fn-name (value)
	   (,set-name (make-instance ',name) value))
	 (defun ,name (fp)
	   ;; There's got to be a better way...
	   (declare (optimize speed))
	   (* ,delta
	      (round (* (,raw-name fp)
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
	 (defmethod f* ((value ,name) &rest rest)
	   (multiple-value-bind (quotient remainder)
	       (round (apply #'* (,raw-name value)
			     (mapcar (lambda (val)
				       (* (,raw-name val) ,small))
				     rest)))
	     (values (,set-raw-name (make-instance ',name)
				    quotient)
		     (* remainder ,small))))
	 ;; * and - are not as simple
	 ;; v = i1 * small
	 ;; v1 * v2 == i1 * small * i2 * small

	 ',name))))
