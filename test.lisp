(defpackage #:fixed/test
  (:use #:cl #:fixed #:5am))

(in-package #:fixed/test)

(def-suite suite :description "All FIXED fixed-point tests.")
(in-suite suite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defdecimal centi 2)
  (defdecimal centi-norm 2 :low -1 :high 1)

  (defdelta one-third 1/3)

  (defdelta Q7.8 1/256 :low -32768/256 :high 32767/256)
  (defdelta Q2 1/4))

(test decimal
  "Test the centi decimal type."
  (let ((dos (make-centi 2))
	(quatro (make-centi 4))
	(quarter (make-centi 0.25)))
    (is (f= dos (make-centi 2)))
    (is (f= dos (f/ quatro dos)))
    (is (f> quatro dos))
    (is (f< dos quatro))
    (is (f/= dos quatro))
    (is (f= (f* dos quatro) (make-centi 8)))
    (is (f>= quatro quatro dos (make-centi-value 199)))
    (is (= (centi-value quatro) 400))
    (is (= (centi quatro) 4))
    (is (= (centi quarter) 1/4))
    (is (= (centi-value quarter) 25))
    (is (= (small dos) 1/100))
    (is (= (delta dos) 1/100))
    (is (string= (princ-to-string quatro)
		 "#<CENTI 4.00>"))
    (is (string= (princ-to-string quarter)
		 "#<CENTI 0.25>"))))

(test ranged-decimal
  "Test the ranged decimal type"
  (let ((min (make-centi-norm -1))
	(max (make-centi-norm  1))
	(eps (make-centi-norm-value 1)))
    (is (f> max eps min))
    (is (f< min eps max))
    (is (f/= min max eps))
    (signals error (f= max 1))
    (signals error (f= max 100))
    (is (= (centi-norm max) 1))
    (is (= (centi-norm-value max) 100))
    (is (f= eps (make-centi-norm 0.01)))
    (signals error (f+ max eps))
    (signals error (f- min eps))
    (is (string= (princ-to-string (make-centi-norm 0.75))
		 "#<CENTI-NORM 0.75>"))
    (is (string= (princ-to-string max)
		 "#<CENTI-NORM 1.00>"))))

(test delta
  (let ((value (make-one-third -1)))
    (is (f= value (make-one-third -1)))
    
    ;; The next smallest power of two from delta
    (is (= (small value) 1/4))
    (is (= (one-third value) -1))))

(test underlying-representation
  (let ((1p25 (make-Q7.8 1.25))
	(1p5  (make-Q7.8 1.5))
	(1p75 (make-Q7.8 1.75))
	(-1p25 (make-Q7.8 -1.25))
	(-1p5  (make-Q7.8 -1.5))
	(-1p75 (make-Q7.8 -1.75)))
    (is (= (Q7.8-value  1p25) (round (*  1.25 256)) #x0140))
    (is (= (Q7.8-value  1p5)  (round (*  1.5 256))  #x0180))
    (is (= (Q7.8-value  1p75) (round (*  1.75 256)) #x01C0))
    ;Since we're dealing with negative numbers, lets just get the u16
    ;representation...  
    (is (= (ldb (byte 16 0) (Q7.8-value -1p25)) 
	   (ldb (byte 16 0) (round (* -1.25 256)))
	   #xFEC0))
    (is (= (ldb (byte 16 0) (Q7.8-value -1p5))
	   (ldb (byte 16 0) (round (* -1.5 256)))
	   #xFE80))
    (is (= (ldb (byte 16 0) (Q7.8-value -1p75))
	   (ldb (byte 16 0) (round (* -1.75 256)))
	   #xFE40))))

(test rounding
  (let ((five  (make-Q2 5))
	(three (make-Q2 3)))

    ;; Default
    (is (f= (f/ five three)
	    (make-Q2-value 7) ;; (round 20/3)
	    (make-Q2 5/3)
	    (make-Q2 1.652)
	    (make-Q2 1.82)))))

(test floor
  (let ((five  (make-Q2 5))
	(three (make-Q2 3))
	(fixed:*rounding-method* #'floor))
    (is (f= (f/ five three)
	    (make-Q2-value 6)
	    (make-Q2 5/3)
	    (make-Q2 1.74)
	    (make-Q2 1.51)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (install-q-reader))

(test reader
  (is (= 1/4 #Q2 0.25))
  (is (= 1/4 #q100 0.25))
  (is (= 1/10 #qd1 0.1))
  (is (= -1/10 #qd1 -0.1))
  (is (= -1/4 #q2 -0.25))
  ;; are these all equal
  (is (= 0.00006103515625 1/16384 #q14 0.00006103515625)))

(test named-reads
  (is (f= #q q7.8 127.5
          (make-q7.8 127.5)
          (make-q7.8-value (round (* 127.5 256)))))
  (is (f= #q q7.8 -127.5
          (make-q7.8 -127.5)))
  ;; Too big
  (signals error (read-from-string "#q q7.8 128.0"))
  ;; Minimum
  (finishes (read-from-string "#q q7.8 -128.0"))
  ;; Too little
  (signals error (read-from-string "#q q7.8 -128.00390625"))

  ;; Decimal type
  (is (f= #qcenti 1.23 (make-centi 1.23) (make-centi-value 123)))
  (signals error (read-from-string "#qcenti 1.234"))
  (signals error (read-from-string "#qcenti -0.001"))
  (signals error (read-from-string "#qcenti-norm 1.01"))
  (signals error (read-from-string "#qcenti-norm -1.01"))
  (finishes (read-from-string "#qcenti-norm 1.00"))
  (finishes (read-from-string "#qcenti-norm -1.00")))

;;; get it to be tail recursive...
(defun print-decimal-fraction (stream remaining-chars remainder denominator)
  (when (plusp remaining-chars)
    (multiple-value-bind (integral new-remainder) (truncate remainder denominator)
      (unless (and (= 0 integral)
                   (= 0 new-remainder))
        (princ integral stream)
        (print-decimal-fraction stream
                                (1- remaining-chars)
                                (* 10 new-remainder)
                                denominator)))))

(defun rational-to-decimal (rat &optional (maximum-width 50))
  "Return a string representing a rational converted to decimal form up to MAXIMUM-WIDTH characters."
  (let ((den (denominator rat)))
    (with-output-to-string (out)
      (multiple-value-bind (integral remainder) (truncate (numerator rat) den)
        ;; Get the negative sign for less-than-zero values
        (when (and (minusp rat) (zerop integral))
          (princ "-" out))
        (princ integral out)
        (let ((remaining-chars (file-position out)))
          (if (< remaining-chars maximum-width)
              (progn
                (princ "." out)
                (if (and (zerop remainder)
                         (plusp (- maximum-width remaining-chars 1)))
                    (princ 0 out)
                    (print-decimal-fraction out
                                            (- maximum-width remaining-chars 1)
                                            (* 10 (abs remainder))
                                            den)))
              (error "Over ~A characters required for integral part of rational" maximum-width)))))))

(defun generate-twos-complement-test (total-bits fractional-bits)
  "Return 3 values:
1 A list of the reader strings for the entire enumeration of valid values
2 A string for one over the maximum limit
3 A string for one under the minimum limit"
  (let* ((front (- total-bits fractional-bits 1))
         (qspec (format nil "#q~d.~d" front fractional-bits))
         (minimum (- (expt 2 (1- total-bits))))
         (maximum (1- (expt 2 (1- total-bits))))
         (denominator (expt 2 fractional-bits)))
    ;; (print minimum)
    ;; (print maximum)
    (values
     (loop for value from minimum to maximum
        collect (format nil "~A ~A" qspec (rational-to-decimal (/ value denominator))))
     ;; Maximum+1
     (format nil "~A ~A" qspec (rational-to-decimal (/ (1+ maximum) denominator)))
     ;; Minimum-1
     (format nil "~A ~A" qspec (rational-to-decimal (/ (1- minimum) denominator))))))

(defun test-all-q-reader-values (total-bits fractional-bits)
  (multiple-value-bind (valid-values max+1 min-1)
      (generate-twos-complement-test total-bits fractional-bits)
    (dolist (value-string valid-values)
      ;(format t "~&testing ~A " value-string)
      (finishes (read-from-string value-string)))
    (signals fixed:q-reader-invalid-value (read-from-string max+1))
    (signals fixed:q-reader-invalid-value (read-from-string min-1))))

(test reader-overflow-3bit
  "Verify the reader for different 3-bit ranges."
  (test-all-q-reader-values 3 1)
  (test-all-q-reader-values 3 2)
#+nil-because-it-generates-negative-spec  (test-all-q-reader-values 3 3))

(test reader-overflow-8bit
  "Verify the reader for different 8-bit ranges."
  (test-all-q-reader-values 8 1)
  (test-all-q-reader-values 8 3)
  (test-all-q-reader-values 8 4)
#+nil-because-it-generates-negative-spec  (test-all-q-reader-values 8 8))
