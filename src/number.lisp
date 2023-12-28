;;; -*- mode:lisp;  coding:utf-8 -*-

#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                    Copyright © 2023  @vlad-km
            /     \                   
            )     (                   
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL >= version 0.8.2  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "NUMBER")
    (defpackage :number
      (:use :cl)
      (:export #:parse-int         #:parse-float
               #:number->fixed     #:string->number
               #:number->Exponential 
               #:to-precision       #:to-string))))

(in-package :number) 

;;;  returns a floating point number.
;;; If a number cannot be parsed from the argument, it returns JS NaN
;;; Leading whitespace in this string is ignored
;;; Ex:   (parse-float "123.12345678")
;;;        => 123.12345678
;;;        (parse-float "123e-2")
;;;        => 1.23
;;;        (parse-float "123e-12")
;;;         => 1.23e-10
(defun parse-float (string)
  (#j:parseFloat string))

;;;  parses a string argument and returns an integer
;;;  of the specified radix or base.
;;; Ex: (parse-int "12345")
;;;     => 12345
;;;     (parse-int "10101" 2)
;;;     => 21
;;;     (parse-int "12345" 8)
;;;     => 5349
;;;     (parse-int "12345" 16)
;;;     => 74565
(defun parse-int (string &optional (radix 10))
  (#j:parseInt string radix))

;;; formats this number using fixed-point notation
;;; ex: (number->fixed 123.123456)
;;;     => "123"
;;;     (number->fixed 123.123456 1)
;;;     => "123.1"
;;;     (number->fixed 123.123456 2)
;;;     => "123.12"
;;;     (number->fixed 123.123456 4)
;;;     => "123.1235"
(defun number->fixed (number &optional (fixnum (ffi:js-null)))
  (ffi:bind-call ((ffi:cl->js number) "toFixed") fixnum))

;;; returns primitive values of type Number
;;; Ex:
;;;   (string->number "1234.567e-10")
;;;     => 1.234567e-7
;;;   (string->number "1234.567e")
;;;     => NaN
;;;   (string->number "1234.567")
;;;     => 1234.567
;;;   (type-of *)
;;;     => FLOAT
;;;   (string->number "1234")
;;;     => 1234
;;;   (type-of *)
;;;     => INTEGER
;;;   (string->number "1234.")
;;;     => 1234
;;;   (type-of *)
;;;     => INTEGER
;;;   (string->number "1234.0")
;;;     => 1234
;;;   (type-of *)
;;;     => INTEGER
;;;   (string->number "1234.0000000001")
;;;     => 1234.0000000001
;;;   (type-of *)
;;;     => FLOAT
(defun string->number (string)
  (#j:Number string))

;;; returns a string representing this number in exponential notation
;;; Ex:
;;;   (number->exponential 123456)
;;;     => "1e+5"
;;;   (number->exponential 123456 2)
;;;     => "1.23e+5"
;;;   (number->exponential 123456 4)
;;;     => "1.2346e+5"
;;;   (number->exponential 123456 1)
;;;     => "1.2e+5"
(defun number->Exponential (number &optional (fraction (ffi:js-null)))
  (ffi:call ((ffi:cl->js number) "toExponential") fraction))

;;;  returns a string representing this number to the specified precision
;;;   If the precision argument is a non-integer value, it is rounded to the nearest integer
;;;  Ex:
;;;
(defun to-precision (number &optional (precision 1))
  (ffi:call ((ffi:cl->js number) "toPrecision") precision))

;;; returns a string representing this number value by radix
(defun to-string (number &optional (radix 10))
  (ffi:call ((ffi:cl->js number) "toString") radix))

(push :number *features*)

(in-package :cl-user)
;;; EOF
