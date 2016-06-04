# FIXED

A Common Lisp fixed-point numeric type package intended to be similar to the Ada language type.  The focus is providing a useful abstraction for known reliable precision in a specific range.  This package uses CLOS to encapsulate the underlying type.

Also provided is a utility package (:fixed/real-time) providing a portable fixed-point type representing the internal real time.

## Syntax

**defdelta** *name delta [:small small-value] [:low low-value] [:high high-value]*

=> *delta-name*

**defdecimal** *name power [:low low-value] [:high high-value]*

=> *decimal-name*

### Arguments
*name* --- a symbol

*delta* --- real number

*power* --- integer

*small-value*, *low-value*, and *high-value* --- optional real numbers

### Description

**defdelta** defines a fixed-point number type named *name* capable of representing a value with at least the accuracy provided in *delta*.

If *small-value* is provided in **defdelta**, it must be a real value no greater than *delta*.  *small-value* is used as the minimum resolution scaling factor for the underlying value.  When *small-value* is not provided, it will be chosen automatically and will be no larger than *delta*.  

The *small-value* can be any real number, but rationals are recommended to avoid unexpected rounding behaviors for some of the operations.  If necessary, consider entering a decimal value using the provided *#Q* reader macro.  The following are equivalent.

```lisp
(defdelta a-fixed-type #qd 0.2 :small #qd 0.2)
(defdelta a-fixed-type 1/5 :small 1/5)
```

**defdecimal** defines a fixed-point number type named *name* capable of representing a base-10 decimal value with up to *power* number of digits to the right of the decimal.  The *small-value* selected will be (expt 10 (- *power*)).  *Note: This declaration is different from the Ada decimal type where you must still define the delta (but as a power-of-10), and you define the number of digits to use in the underlying type.*

*low-value* and *high-value* are both optional for **defdelta** or **defdecimal**, and are used to define the most-negative and most-positive values of the fixed point type.

**defdecimal** is essentially identical to **defdelta** when called with an identical *delta* and *small* that is a power of 10.  The only difference is that values that have a **defdecimal** defined type will always be printed in decimal form.  Values with a **defdelta** defined type may be printed as rationals.

### Type Operations
**defdelta** and **defdecimal** create a set of functions and generic methods associated with *name*.

| Operation | Type | Description |
| --- | --- | --- |
| (make-*name* value)            | Function | Return a new instance of *name* with value rounded as necessary with \*rounding-method\* |
| (make-*name*-value value)      | Function | Return a new instance of *name* with the provided underlying value |
| (*name* fp)                    | Function | Return the value in the *name* instance scaled by *small* |
| (*name*-value fp)              | Function | Returns the underlying value of an instance of *name* |
| (set-*name* fp value)          | Generic  | Set the value of a *name* instance, rounding as necessary with \*rounding-method\* |
| (set-*name*-value fp value)    | Function | Set the underlying integer value of an instance of *name* |
| (setf (*name* fp) value)       | setf     | Set the value of fp with rounding as necessary with \*rounding-method\* |
| (setf (*name*-value fp) value) | setf     | Set the underlying value of fp |
| (small fp) or (small '*name*)  | Generic  | Return the *small* when passed '*name* or an instance of *name* |
| (delta fp) or (delta '*name*)  | Generic  | Return the *delta* when passed '*name* or an instance of *name* |

### Math Operations
Generic Function Predicates:
f=
f/=
f>
f>=
f<
f<=

Generic Arithmetic Operations:
f+
f-
f*
f/

## Usage
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

## Fixed-point Reader Macro

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

> #Q7.8 -256.0
-256
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

## Future Work
- Fixed-point reader macro improvements
  - Read into a superclass of defined delta types
- Determine if the second return value from rounding operations is in the best form.

# FIXED/REAL-TIME

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

# License

MIT
