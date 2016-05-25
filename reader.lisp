;;; A reader macro for fixed-point

(in-package #:fixed)

(unless (boundp '+whitespace+)
  (defconstant +whitespace+ '(#\space #\tab #\linefeed #\return #\page)))

(defun clear-whitespace (stream)
  (do ((char (peek-char nil stream nil nil)
	     (peek-char nil stream nil nil)))
      ((not (member char +whitespace+)))
    (read-char stream nil nil)))

(defun read-sign (stream)
  (if (char= #\- (peek-char nil stream))
      (progn
        (read-char stream)
        -1)
      1))

(defun read-integer (stream &optional (check-sign? t))
  "Read a base-10 integer, leaving whitespace or other characters on
the stream.  The integer is terminated by any non-digit-char.
Return is (values sign result power terminating-char)"
  (let ((result 0)
	(sign (if check-sign?
                  (read-sign stream)
                  1)))
    (loop for char = (peek-char nil stream nil nil)
	 for power from 0
       while (and (characterp char) (digit-char-p char))
       do (setf result (+ (* 10 result)
			  (- (char-code (read-char stream nil nil))
			     (char-code #\0))))
       finally (return (values sign result power char)))))

(defun read-q-decimal (stream)
  "This reads the integer and fraction into the car and cdr of a cons.
  The second value is a cons of the count of digits in the first and
  second integers. "
  (multiple-value-bind (sign value power term) (read-integer stream)
    (multiple-value-prog1
	(if (char= term #\.)
	    ;; This is a two-part spec
	    (progn
              ;; Eliminate the decimal point
	      (read-char stream)
	      ;; Enforce #.#
	      (assert (digit-char-p (peek-char nil stream)))
	      (multiple-value-bind (fsign fraction dpower)
		  (read-integer stream nil)
                (declare (ignore fsign))
		(values (cons (* sign value) (* sign fraction))
			(cons power dpower))))
	    ;; Return the 1-part spec
	    (values (cons nil (* sign value))
		    (cons nil power)))
      ;; Clean up the rest
      (clear-whitespace stream))))

(defun spec-small (q-spec)
  "Given a cons of a Q spec (see Q-READER), return the inverse of the
  small-value of a corresponding fixed type."
  (expt 2 (cdr q-spec)))

;; TODO: decimal #QD3 maybe
;; TODO: unsigned #QU3 maybe
(defun q-reader (stream subchar arg)
  "A Q spec looks like this:

#Q3   => a type with delta and small of (/ (expt 2 3))
Return value (cons nil 3)

#Q7.8 => a 16 bit (1+ 7 8) signed type with delta and small of (/ (expt 2 8))
Return value (cons 7 8)"
  (declare (ignore subchar arg)
	   (optimize debug))
  (let ((spec  (read-q-decimal stream)))
    (multiple-value-bind (value sizes) (read-q-decimal stream)
      ;; Construct the ratio
      (let* ((ratio (+ (car value)
		       (/ (cdr value)
			  (expt 10 (cdr sizes)))))
	     ;; Generate the spec-based underlying value
	     (raw (* ratio (spec-small spec))))
	;; Verify the ratio meets the spec
	(if (and (integerp raw)
		 ;; If it's a two-part spec...
		 (or (not (car spec))
		     ;; and fits in the spec
		     (<= (integer-length raw)
			 (+ 1 (car spec) (cdr spec)))))
	    ratio
	    (error "~D.~v,'0D is not a #Q~:[~D~;~:*~D.~D~]"
		   (car value) (cdr sizes) (abs (cdr value))
		   (car spec)  (cdr spec)))))))

(defun install-q-reader ()
  "A Q spec looks like this:
#Q3   => a type with delta and small of (/ (expt 2 3))
#Q7.8 => a 16 bit (1+ 7 8) signed type with delta and small of (/ (expt 2 8))

Use the Q reader to input fixed-point literals in decimal form.  The
rightmost integer in the Q spec defines the number of fractional
bits.  The left-most number before the period, if provided, defines
the number of non-fractional bits.  The sign bit is implied.  

e.g.
#Q3 1.5    => 3/2
#Q3 1.25   => 5/4
#Q3 1.125  => 9/8
#Q3 1.0625 => Error: 1.0625 is not a Q3

The error in the last one is because 1.0625 requires 4 fractional bits
to represent.  

#Q4 1.0625 => 17/16

Currently, the reader function returns a ratio representing the
  decimal value read."
  (set-dispatch-macro-character #\# #\Q #'q-reader))
