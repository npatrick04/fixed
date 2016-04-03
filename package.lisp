;;;; package.lisp

(defpackage #:fixed
  (:use #:cl)
  (:shadow #:float)
  (:export

   #:spec
   #:range-spec
   #:fp
   #:ordinary-fp
   #:value

   #:get-fixed-type
   #:defdelta

   ;; Conversion
   #:float))

