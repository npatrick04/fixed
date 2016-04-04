;;;; package.lisp

(defpackage #:fixed
  (:use #:cl)
  (:shadow #:float)
  (:export
   #:defdelta

   ;; Conversion
   #:float))

