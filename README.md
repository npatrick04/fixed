FIXED
=====

A Common Lisp fixed-point numeric type package intended to be similar to the Ada language type.  The focus is providing a useful abstraction for known reliable precision in a specific range.  This package uses CLOS to encapsulate the underlying type.  It does not provide an efficient mechanism for implementing SIMD operations.

Also provided is a utility package (:fixed/real-time) providing a portable fixed-point type representing the internal real time.

Usage
=====

```lisp
;; Ordinary power-of-2 fixed point type that supports a resolution of 1/10.
;; This is represented by a 1/16 resolution value.
> (defdelta foo 1/10)

;; Fixed point type with precise resolution
;; This is represented by a 1/10 resolution value.
> (defdelta bar 1/10 :small 1/10)

;; Adding range info
> (defdelta foobar 0.01 :small 0.01 :low 0.00 :high 1.00)
> (defparameter fb (make-foobar 0.5))
FB

> fb
#<FOOBAR 0.5>

> (f+ fb (make-foobar 1/2))
#<FOOBAR 1.0>
> (f+ fb (make-foobar 0.51))
;; ERROR: The value 101 is not of type (MOD 101).

> (setf (foobar fb) 0.49)
#<FOOBAR 0.48999998>
> (f+ fb (make-foobar 0.51))
#<FOOBAR 1.0>
```

FIXED/REAL-TIME
===============

A utility package that implements a fixed-point type for internal real time.

```lisp
;; Get the current internal real time as a fixed point
> (defparameter the-time (current-time))
THE-TIME
> the-time
#<REAL-TIME 3711125.080>

;; do some stuff

;; calculate deltat
> (f- (current-time) the-time)
#<REAL-TIME 15.616>
```

Fixed-point Reader Macro
========================

A fixed-point reader macro provides a method to input fixed-point literals in decimal form.  The reader macro uses the Q format to define a fixed-point spec for the following value.

Install the reader macro as a Q dispatch on # with `(install-q-reader)`.

e.g.

```lisp
;; Read in fixed-point literals that can be represented exactly by a Q8 spec.
> #Q8 1.5
3/2

> #Q8 0.0078125
1/128

;; Read in a fixed-point literal that can be represented exactly by a Q3 spec, and one that can't.
> #Q3 1.5
3/2

> #Q3 0.0078125
;; ERROR: 0.0078125 is not a #Q3
```

Bounds checking can also be performed when the maximum number of useable bits are provided in the Q spec.

```lisp
;; Read in the most positive Q7.8 value.
> #Q7.8 255.99609375
65535/256

> #Q7.8 256.0
;; Error: 256.0 is not a #Q7.8
```

Decimal fixed-point values can be read as well with `#QD` and an optional spec value for digits.

e.g.
```lisp
> #QD 1.2345678901234567890
1234567890123456789/1000000000000000000

> #QD3 1.2345678901234567890
;; ERROR: 1.2345678901234567890 is not a #QD3

> #QD3 1.234
617/500
> (float *)
1.234
```

Future Work
===========
- Fixed-point reader macro improvements
  - Read into a superclass of defined delta types
- Determine if the second return value from rounding operations is in the best form.

License
=======

MIT
