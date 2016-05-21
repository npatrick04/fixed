;;; A reader macro for fixed-point

(in-package #:fixed)

(unless (boundp '+whitespace+)
  (defconstant +whitespace+ '(#\space #\tab #\linefeed #\return #\page)))

(defun clear-whitespace (stream)
  (do ((char (peek-char nil stream nil nil)
	     (peek-char nil stream nil nil)))
      ((not (member char +whitespace+)))
    (read-char stream nil nil)))

(defun read-integer (stream)
  "Read a base-10 integer, leaving whitespace or other characters on
the stream.  The integer is terminated by any non-digit-char."
  (let ((result 0))
    (loop for char = (peek-char nil stream nil nil)
	 for power from 0
       while (digit-char-p char)
       do (setf result (+ (* 10 result)
			  (- (char-code (read-char stream nil nil))
			     (char-code #\0))))
       finally (return (values result power char)))))

(defun read-q-decimal (stream)
  "This reads the integer and fraction into the car and cdr of a cons.
  The second value is a cons of the count of digits in the first and
  second integers. "  
  (multiple-value-bind (value power term) (read-integer stream)
    (multiple-value-prog1
	(if (char= term #\.)
	    ;; This is a two-part spec
	    (progn
	      (read-char stream)
	      ;; Enforce #.#
	      (assert (digit-char-p (peek-char nil stream)))
	      (multiple-value-bind (decimal dpower)
		  (read-integer stream)
		(values (cons value decimal)
			(cons power dpower))))
	    ;; Return the 1-part spec
	    (values (cons nil value)
		    (cons nil power)))
      ;; Clean up the rest
      (clear-whitespace stream))))

(defun spec-small (q-spec)
  "Given a cons of a Q spec (see Q-READER), return the inverse of the
  small-value of a corresponding fixed type."
  (expt 2 (cdr q-spec)))

(defun q-reader (stream subchar arg)
  "A Q spec looks like this:

\"#Q3\"   => a type with delta and small of (/ (expt 2 3))
Return value (cons nil 3)

\"#Q7.8\" => a 16 bit (1+ 7 8) signed type with delta and small of (/ (expt 2 8))
Return value (cons 7 8)

TODO: decimal #QD3 maybe
TODO: unsigned #QU3 maybe
"
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
		   (car value) (cdr sizes) (cdr value)
		   (car spec)  (cdr spec)))))))

;; (defun install-q-reader ()
;;   (set-dispatch-macro-character #\# #\Q #'q-reader))
(set-dispatch-macro-character #\# #\Q #'q-reader)
