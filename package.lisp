;;;; package.lisp

(defpackage #:fixed
  (:use #:cl)
  (:export
   ;; The big kahuna
   #:defdelta

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

