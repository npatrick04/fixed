(defpackage #:fixed/test
  (:use #:cl #:fixed #:5am))

(in-package #:fixed/test)

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

(defdelta foo 1/3)

(test delta
  (let ((value (make-foo -1)))
    (is (f= value (make-foo -1)))
    
    ;; The next smallest power of two from delta
    (is (= (small value) 1/4))
    (is (= (foo value) -1))))

