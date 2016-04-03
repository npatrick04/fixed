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
  (declare (type range-spec spec)
           (type number value))
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
  ((spec :reader spec
         :initarg :spec)))

(defclass ordinary-fp (fp)
  ((value :accessor value
          :initarg :value
          :type integer)))

(defparameter *specs* (make-hash-table))
(defun get-fixed-type (name)
  (gethash name *specs*))

(defmacro defdelta (name delta &key range small)
  (let ((spec (gensym "SPEC"))
	(fn-name (intern (concatenate 'string
                                      "MAKE-"
                                      (symbol-name name))))
        (small (if small small
                   (expt 2 (- (integer-length (ceiling (/ delta))))))))
    `(let ((,spec (make-instance ,(if range
				      'ordinary-ranged-fp-spec
				      'ordinary-fp-spec)
                                 :name ',name
                                 :low ,(car range)
                                 :high ,(cadr range)
                                 :delta ,delta
                                 :small ,small)))
       ;; (defclass ,name ()
       ;; 	 ((value :accessor value
       ;; 		 :initarg :value
       ;; 		 :type (integer ,))))
       (setf (gethash ',name *specs*)
             ,spec)
       (defun ,fn-name (value)
         (assert (range-check ,spec value))
         (make-instance 'ordinary-fp
                        :spec ,spec
                        :value (round value ,small)))
       ,spec)))

(defun float (delta)
  "There's got to be a better way..."
  (declare (optimize speed))
  (cl:float (* (delta (spec delta))
               (round (* (value delta)
                         (small (spec delta)))
                      (delta (spec delta))))))

(defmethod print-object ((object ordinary-fp) stream)
  (print-unreadable-object (object stream)
    (format stream "~A ~D"
            (spec-name (spec object))
            (float object))))
