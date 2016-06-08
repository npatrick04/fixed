;;;; package.lisp

(defpackage #:fixed
  (:use #:cl)
  (:export
   ;; The big kahuna
   #:defdelta
   #:defdecimal

   ;; The types
   #:fp
   #:ordinary-fp
   #:ordinary-ranged-fp

   ;; Type Predicates
   #:fixedp
   #:ranged-fixedp
   #:decimal-fixedp
   #:ordinary-fixedp

   ;; A couple accessors
   #:small
   #:delta
   #:size

   ;; Predicates
   #:f= 
   #:f/=
   #:f> 
   #:f>=
   #:f< 
   #:f<=

   ;; Rounding can be controlled with this...
   #:*rounding-method*
   
   ;; Arithmetic
   #:f+
   #:f-
   #:f*
   #:f/

   ;; Reader
   #:install-q-reader
   ))

