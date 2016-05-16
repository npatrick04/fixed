(defpackage #:fixed/test
  (:use #:cl #:fixed #:5am))

(in-package #:fixed/test)

(def-suite suite :description "All FIXED fixed-point tests.")
(in-suite suite)

(defdecimal centi 2)
(defdecimal centi-norm 2 :low -1 :high 1)

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

(defdelta one-third 1/3)

(test delta
  (let ((value (make-one-third -1)))
    (is (f= value (make-one-third -1)))
    
    ;; The next smallest power of two from delta
    (is (= (small value) 1/4))
    (is (= (one-third value) -1))))

(defdelta Q7.8 1/256 :low -32768/256 :high 32767/256)
(defdelta Q2 1/4)

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

