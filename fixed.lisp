;;;; fixed.lisp

(in-package #:fixed)

;;; "fixed" goes here. Hacks and glory await!

(defclass spec ()
  ((name :reader spec-name
         :initarg :name)))

(defclass range-spec (spec)
  ((low :reader low
        :initarg :low
        :type real)
   (high :reader high
         :initarg :high
         :type real)))
(defmethod print-object ((rs range-spec) stream)
  (print-unreadable-object (rs stream)
    (format stream "<= ~A val ~A"
            (low rs) (high rs))))

(declaim (inline range-check))
(defun range-check (spec value)
  ;; (declare (type range-spec spec)
  ;;          (type number value))
  (<= (low spec) value (high spec)))

(defclass ordinary-fp-spec ()
  ((small :reader small
          :initarg :small
          :type :real)
   (delta :reader delta
          :initarg :delta
          :type real)))
(defmethod print-object ((spec ordinary-fp-spec) stream)
  (print-unreadable-object (spec stream)
    (format stream "delta ~A : ~A"
            (delta spec) (small spec))))

(defclass ordinary-ranged-fp-spec (ordinary-fp-spec ranged)
  ())

(defclass fp ()
  ())

(defclass ordinary-fp (fp)
  ())

(defclass ordinary-ranged-fp (fp)
  ())

(defparameter *specs* (make-hash-table))
(defun get-fixed-type (name)
  (gethash name *specs*))

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
