;;;; package.lisp

(defpackage #:fixed
  (:use #:cl)
  (:export
   ;; The big kahuna
   #:defdelta
   #:defdecimal

   ;; Predicates
   #:f= 
   #:f/=
   #:f> 
   #:f>=
   #:f< 
   #:f<=
   
   ;; Arithmetic
   #:f+
   #:f-
   #:f*))

