;;; A reader macro for fixed-point

(in-package #:fixed)

(define-condition q-reader-error (error)
  ())

(define-condition q-reader-unknown-fixed-type (q-reader-error)
  ((symbol :reader unknown-fixed-type-symbol
           :initarg :symbol)))

(define-condition q-reader-non-symbol (q-reader-error)
  ())

(define-condition q-reader-invalid-value (q-reader-error)
  ((message :reader q-reader-invalid-value-message
            :initarg :message))
  (:report (lambda (c s)
             (princ (q-reader-invalid-value-message c) s))))

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
  (declare (type (cons t integer) q-spec))
  (expt 2 (cdr q-spec)))

(defun q-ratio (value sizes)
  (+ (car value)
     (/ (cdr value)
        (expt 10 (cdr sizes)))))

(defun ratio-meets-spec? (raw spec)
  (and (integerp raw)
       ;; If it's a two-part spec...
       (or (not (car spec))
           ;; and fits in the spec
           (<= (integer-length raw)
               (+ (car spec) (cdr spec))))))

(defun read-ordinary-q-decimal (stream spec)
  "Read an ordinary fixed-point value."
  (declare (optimize debug))
  (multiple-value-bind (value sizes) (read-q-decimal stream)
    ;; Construct the ratio
    (let* ((ratio (q-ratio value sizes))
           ;; Generate the spec-based underlying value
           (raw (* ratio (spec-small spec))))
      ;; Verify the ratio meets the spec
      (if (ratio-meets-spec? raw spec)
          ratio
          (error 'q-reader-invalid-value
                 :message
                 (format nil "~D.~v,'0D is not a #Q~:[~D~;~:*~D.~D~]"
                         (car value) (cdr sizes) (abs (cdr value))
                         (car spec)  (abs (cdr spec))))))))

(defun read-ordinary-q (stream)
  "Read an ordinary fixed-point value."
  (read-ordinary-q-decimal stream (read-q-decimal stream)))

(defun read-decimal-q-decimal (stream spec spec-sizes)
  "Read a decimal #QD with infinite precision, or #QDvalue where value
is some integer defining the number of digits of precision."
  (declare (optimize debug))
  (multiple-value-bind (value sizes) (read-q-decimal stream)
    ;; Construct the ratio
    (let* ((ratio (q-ratio value sizes)))
      ;; If there is a decimal digit spec value...
      (if (plusp (cdr spec-sizes))
          ;; Generate the spec-based underlying value
          (let ((raw (* ratio (expt 10 (cdr spec)))))
            ;; Verify the ratio meets the spec
            (if (ratio-meets-spec? raw spec)
                ratio
                (error "~D.~v,'0D is not a #QD~:[~D~;~:*~D.~D~]"
                       (car value) (cdr sizes) (abs (cdr value))
                       (car spec)  (cdr spec))))
          ;; Otherwise just return the full-precision value.
          ratio))))

(defun read-decimal-q (stream)
  "Read a decimal #QD with infinite precision, or #QDvalue where value
is some integer defining the number of digits of precision."
  (declare (optimize debug))
  (multiple-value-bind (spec spec-sizes) (read-q-decimal stream)
    (read-decimal-q-decimal stream spec spec-sizes)))

(defun read-q-character-spec (stream)
  "Read a user-specified Q type, or the D for a decimal type.
If the first character read is #\D or #\d, followed by a digit
character, then the result is 'D, indicating a decimal type.

Otherwise, the character is unread, the READ function is called,
returning the resultant symbol"
  (let ((first-char (read-char stream)))
    (if (and (char= (char-upcase first-char) #\D)
             (digit-char-p (peek-char nil stream)))
        'd
        (progn
          (unread-char first-char stream)
          (read stream)))))

(defun read-user-decimal (decimal-type stream)
  (let* ((make-name (find-symbol (concatenate 'string "MAKE-" (symbol-name decimal-type))
                                 (symbol-package decimal-type)))
         (small (small decimal-type))
         (spec  (cons nil (round (log (/ small) 10))))
         (sizes (cons nil (length (format nil "~D" (cdr spec))))))
    (let ((ratio (read-decimal-q-decimal stream spec sizes)))
      (funcall (symbol-function make-name) ratio))))

(defun read-user-ordinary (ordinary-type stream)
  (let* ((make-name (symb "MAKE-" (symbol-name ordinary-type)))
         (small (small ordinary-type))
         (spec (cons nil (round (log (/ small) 2)))))
    (let ((ratio (read-ordinary-q-decimal stream spec)))
      (funcall (symbol-function make-name) ratio))))

;; TODO: unsigned #QU3 maybe
(defun q-reader (stream subchar arg)
  "A generic Q spec looks like this:

#Q3   => a type with delta and small of (/ (expt 2 3))
Return value (cons nil 3)

#Q7.8 => a 16 bit (1+ 7 8) signed type with delta and small of (/ (expt 2 8))
Return value (cons 7 8)

Alternatively, a generic Q spec could be in decimal form:

#QD2 123.45 => 12345/100"
  (declare (ignore subchar arg))
  (let ((first-char (peek-char nil stream)))
    (if (or (digit-char-p first-char)
            (char= first-char #\-))
        (read-ordinary-q stream)
        ;; It must be either a user-defined fixed type or a decimal type
        (let ((the-type (read-q-character-spec stream)))
          (if (symbolp the-type)
              (cond
                ((decimal-fixedp the-type)
                 (read-user-decimal the-type stream))
                ((ordinary-fixedp the-type)
                 (read-user-ordinary the-type stream))
                ((eq the-type 'd)
                 (read-decimal-q stream))
                (t
                 (error 'q-reader-unknown-fixed-type :symbol the-type)))
              (error 'q-reader-non-symbol))))))

(defun install-q-reader (&optional (readtable *readtable*))
  "The Q reader can be used to read fixed point types directly with
exact precision.  Generic fixed point types will be read directly
as rational values, and are suitable as an argument for a compatible
fixed point type using the MAKE-NAME constructor, where NAME is
the name of the fixed point type.

A generic ordinary fixed point Q spec looks like this:
#Q3   => a type with delta and small of (/ (expt 2 3))
#Q7.8 => a 16 bit (1+ 7 8) signed type with delta and small of (/ (expt 2 8))

A generic decimal fixed point Q spec looks like this:
#QD3   => a type with delta and small of 1/1000
#QD3.1 => a 5 bit type with delta and small of 1/10, i.e. min == -

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
  (set-dispatch-macro-character #\# #\Q #'q-reader readtable))
