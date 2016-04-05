;;;; fixed.lisp

(in-package #:fixed)

(defclass fp ()
  ())

(defclass ordinary-fp (fp)
  ())

(defclass ordinary-ranged-fp (fp)
  ())

(defgeneric float (delta))

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
                   (expt 2 (- (integer-length (ceiling (/ delta))))))))
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
		`(setf (%value fp) value))
	   fp)
	 (defun ,set-name (fp value)
	   (,set-raw-name fp (round value ,small)))
	 (defun ,fn-name (value)
	   (,set-name (make-instance ',name) value))
	 (defmethod float ((fp ,name))
	   ;; There's got to be a better way...
	   (declare (optimize speed))
	   (cl:float (* ,delta
			(round (* (,raw-name fp)
				  ,small)
			       ,delta))))

	 (defmethod print-object ((object ,name) stream)
	   (print-unreadable-object (object stream :type t)
	     (format stream "~A" (float object))))

	 (defsetf ,name ,set-name)
	 (defsetf ,raw-name ,set-raw-name)
	 
	 ',name))))
