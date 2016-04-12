(defpackage #:fixed/real-time
  (:use #:cl #:fixed)
  (:export
   ;; The class, and the engineering-units reader.  setf'able
   #:real-time

   ;; Engineering units
   #:make-real-time
   #:set-real-time

   ;; Counts
   #:make-real-time-value
   #:set-real-time-value

   ;; Reader of the internal count, setf'able
   #:real-time-value

   ;; Helper functions
   current-time
   set-current-time))

(in-package #:fixed/real-time)

(defdecimal real-time (round (log INTERNAL-TIME-UNITS-PER-SECOND 10)))

(declaim (inline current-time set-current-time))
(defun current-time ()
  "Create a new real-time instance with the current internal real time."
  (make-real-time-value (get-internal-real-time)))
(defun set-current-time (time)
  "Update a real-time instance with the current internal real time."
  (setf (real-time-value time) (get-internal-real-time)))

;; (LET ((DELTA (EXPT 10 (- 3))))
;;   (DECLARE (TYPE RATIONAL DELTA))
;;   (DEFDELTA REAL-TIME DELTA :RANGE NIL :SMALL DELTA)
;;   (DEFMETHOD PRINT-OBJECT ((FIXED::OBJECT REAL-TIME) STREAM)
;;     (PRINT-UNREADABLE-OBJECT (FIXED::OBJECT STREAM :TYPE T)
;;       (MULTIPLE-VALUE-BIND (FIXED::QUOTIENT FIXED::REMAINDER)
;;           (TRUNCATE (REAL-TIME-VALUE FIXED::OBJECT) (/ DELTA))
;;         (FORMAT STREAM "~D.~v,,,'0<~D~>" FIXED::QUOTIENT 3
;;                 (ABS FIXED::REMAINDER))))))

;; (LET ((DELTA (EXPT 10 (- 3))))
;;   (DECLARE (TYPE RATIONAL DELTA))
;;   (PROGN
;;     (DEFCLASS REAL-TIME (FIXED::ORDINARY-FP)
;;       ((FIXED::VALUE :READER REAL-TIME-VALUE :WRITER (SETF FIXED::%VALUE)
;;                      :INITARG :VALUE :TYPE INTEGER)))
;;     (DEFUN SET-REAL-TIME-VALUE (FIXED::FP FIXED::VALUE)
;;       (SETF (FIXED::%VALUE FIXED::FP) (THE INTEGER FIXED::VALUE))
;;       FIXED::FP)
;;     (DEFUN SET-REAL-TIME (FIXED::FP FIXED::VALUE)
;;       (SET-REAL-TIME-VALUE FIXED::FP (ROUND FIXED::VALUE DELTA)))
;;     (DEFUN MAKE-REAL-TIME (FIXED::VALUE)
;;       (SET-REAL-TIME (MAKE-INSTANCE 'REAL-TIME) FIXED::VALUE))
;;     (DEFUN MAKE-REAL-TIME-VALUE (FIXED::VALUE)
;;       (SET-REAL-TIME-VALUE (MAKE-INSTANCE 'REAL-TIME) FIXED::VALUE))
;;     (DEFUN REAL-TIME (FIXED::FP)
;;       (DECLARE (OPTIMIZE SPEED))
;;       (* DELTA (ROUND (* (the integer (REAL-TIME-VALUE FIXED::FP)) DELTA) DELTA)))
;;     (DEFMETHOD PRINT-OBJECT ((FIXED::OBJECT REAL-TIME) STREAM)
;;       (PRINT-UNREADABLE-OBJECT (FIXED::OBJECT STREAM :TYPE T)
;;         (FORMAT STREAM "~A" (REAL-TIME FIXED::OBJECT))))
;;     (DEFSETF REAL-TIME SET-REAL-TIME)
;;     (DEFSETF REAL-TIME-VALUE SET-REAL-TIME-VALUE)
;;     (FIXED::PREDICATE F= REAL-TIME REAL-TIME-VALUE =)
;;     (FIXED::PREDICATE F/= REAL-TIME REAL-TIME-VALUE /=)
;;     (FIXED::PREDICATE F> REAL-TIME REAL-TIME-VALUE >)
;;     (FIXED::PREDICATE F>= REAL-TIME REAL-TIME-VALUE >=)
;;     (FIXED::PREDICATE F< REAL-TIME REAL-TIME-VALUE <)
;;     (FIXED::PREDICATE F<= REAL-TIME REAL-TIME-VALUE <=)
;;     (FIXED::MATH F+ REAL-TIME SET-REAL-TIME-VALUE REAL-TIME-VALUE +)
;;     (FIXED::MATH F- REAL-TIME SET-REAL-TIME-VALUE REAL-TIME-VALUE -)
;;     (DEFMETHOD F* ((FIXED::VALUE REAL-TIME) &REST REST)
;;       (MULTIPLE-VALUE-BIND (FIXED::QUOTIENT FIXED::REMAINDER)
;;           (ROUND
;;            (APPLY #'* (REAL-TIME-VALUE FIXED::VALUE)
;;                   (MAPCAR
;;                    (LAMBDA (FIXED::VAL) (* (REAL-TIME-VALUE FIXED::VAL) DELTA))
;;                    REST)))
;;         (VALUES (SET-REAL-TIME-VALUE (MAKE-INSTANCE 'REAL-TIME) FIXED::QUOTIENT)
;;                 (* FIXED::REMAINDER DELTA))))
;;     'REAL-TIME)
;;   (DEFMETHOD PRINT-OBJECT ((FIXED::OBJECT REAL-TIME) STREAM)
;;     (PRINT-UNREADABLE-OBJECT (FIXED::OBJECT STREAM :TYPE T)
;;       (MULTIPLE-VALUE-BIND (FIXED::QUOTIENT FIXED::REMAINDER)
;;           (TRUNCATE (REAL-TIME-VALUE FIXED::OBJECT) (/ DELTA))
;;         (FORMAT STREAM "~D.~v,,,'0<~D~>" FIXED::QUOTIENT 3
;;                 (ABS FIXED::REMAINDER))))))
