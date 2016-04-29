FIXED
=====

A Common Lisp fixed-point numeric type package intended to be similar to the Ada language type.  Also provided is a utility package (:fixed/real-time) providing a portable fixed-point type representing the internal real time.

Usage
=====

```lisp
;; Ordinary power-of-2 fixed point type that supports a resolution of 1/10.
;; This is represented by a 1/16 resolution value.
(defdelta foo 1/10)

;; Fixed point type with precise resolution
;; This is represented by a 1/10 resolution value.
(defdelta bar 1/10 :small 1/10)

;; Adding range info
(defdelta foobar 0.01 :small 0.01 :low 0.00 :high 1.00)
(defparameter fb (make-foobar 0.5))

(f+ fb (make-foobar 1/2))   ;; OK
(f+ fb (make-foobar 0.51))  ;; ERROR

(setf (foobar fb) 0.49)
(f+ fb (make-foobar 0.51)) ;; OK
```

FIXED/REAL-TIME
===============

A utility package that implements a fixed-point type for internal real time.

```lisp
;; Get the current internal real time as a fixed point
(defparameter the-time (current-time))
;; => #<REAL-TIME 522918.199>

;; do some stuff

;; calculate deltat
(f- (current-time) the-time)
;; => #<REAL-TIME 15.616>
```

License
=======

MIT
