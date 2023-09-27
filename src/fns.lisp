;;; -*- mode:lisp; coding:utf-8 -*-

#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                                           Copyright 2017,2018,2023  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/moren-electron
            )     (                   2023, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL Moren edition  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#

;;; fns - very approximately FuNctional Style
;;; package for use in the Moren environment
;;; compiling the package as
;;;              (load "pathnames/fns.lisp" :hook bin :output "fns.js")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fns)
    (make-package :fns :use (list 'cl))))

(in-package :fns)

;;; Currying functions
;;; (mapcar (lambda (base) (expt base 2)) '(2 3 4 5 6))
;;; =>
;;;   (4 8 16 32 64)
;;;
;;; (mapcar (compose (curry #'* 2) (curry #'+ 1)) '(1 2 3 4))
;;; => (4 6 8 10)
(export '(fns::curry))
(defun curry (function &rest args)
    (lambda (&rest more-args)
        (apply function (jscl::append-two args more-args))))

(export '(fns::rcurry))
(defun rcurry (function &rest args)
    (lambda (&rest more-args)
        (apply function (jscl::append-two more-args args))))

;;; left association
;;; (reduce (lambda (arg fun) (print (list fun arg ))) '(f1 f2 f3)  :initial-value 'args)
;;;
;;; (foldl (lambda (arg fun) (print (list fun arg ))) 'args '(f1 f2 f3) )
;;; =>
;;;    (F1 ARGS)
;;;    (F2 (F1 ARGS))
;;;    (F3 (F2 (F1 ARGS)))
(export '(fns::foldl))
(defun foldl (fn init seq)
    (reduce fn seq :initial-value init))

;;; right association
;;; (reduce (lambda (fun arg) (print (list fun arg ))) '(f1 f2 f3) :from-end t :initial-value 'args)
;;;
;;; (foldr (lambda (fun arg) (print (list fun arg ))) '(f1 f2 f3) 'args)
;;; =>
;;;   (F3 ARGS)
;;;   (F2 (F3 ARGS))
;;;   (F1 (F2 (F3 ARGS)))
(export '(fns::foldr))
(defun foldr (fn seq init)
    (reduce fn seq :from-end t :initial-value init))

;;; compose functions -  right association
;;; compose g f x
;;;      => (g ( f (x arg)))
;;; (funcall
;;;    (fns:compose
;;;       (lambda (x) (print (list :and-finally x)) (terpri))
;;;       (lambda (x) (print (list :secondly x)) (list :second x))
;;;       (lambda (x) (print (list :firstly x)) (list :first x)))
;;;  'anything)
;;; =>
;;;   (:FIRSTLY ANYTHING) 
;;;   (:SECONDLY (:FIRST ANYTHING)) 
;;;   (:AND-FINALLY (:SECOND (:FIRST ANYTHING)))
(export '(fns::compose))
(defun compose (&rest functions)
  (flet ((%last (lst) (car (the cons (last lst))) ))
    (cond ((null functions)   'identity)
          ((jscl::singleton-p functions) (car functions))
          ((jscl::singleton-p (rest functions))
           (destructuring-bind (fn1 fn2) functions
             (lambda (&rest args)
               (funcall fn1 (apply fn2 args))) ))
          (t (let ((fn1 (%last functions))
                   (fnseq (butlast functions)))
               (lambda (&rest args)
                 (foldr 'funcall fnseq (apply fn1 args)))
               )))))

;;; sequence of functions in java-script style
;;;   f1(args).f2().f3() =>  f3( f2 (f1 args))
;;;
;;; (funcall 
;;;   (fns:chain 
;;;     (lambda (x) (print (list :firstly x)) (list :first x)) 
;;;     (lambda (x) (print (list :secondly x)) (list :second x)) 
;;;     (lambda (x) (print (list :and-finally x)) (terpri))) 
;;;  'anything)
;;; =>
;;;  (:FIRSTLY ANYTHING) 
;;;  (:SECONDLY (:FIRST ANYTHING)) 
;;;  (:AND-FINALLY (:SECOND (:FIRST ANYTHING)))
(export '(fns::chain))
(defun chain (&rest x) (apply 'compose (reverse x)))

;;; combine two funs with binary op
(export '(fns::combine))
(defun combine (op g f)
    (lambda (args)
        (funcall op (funcall g args) (funcall f args))))

;;; last n els from seq
;;; the same as the drop function (see below) but only for the list form 
(export '(fns::lastn))
(defun lastn (n seq)
    (nthcdr (1+ n) seq))

;;; take head length n
;;; vector/list
(export '(fns::take))
(defun take (n seq)
    (subseq seq 0 n))

;;; take tail from position n
;;; vector/list
(export '(fns::drop))
(defun drop (n seq)
        (subseq seq n))

;;; split seq on head tail pair
;;; => (take n seq)
;;;    (drop n seq)
;;; vector/list
(export '(fns::split-seq))
(defun split-seq (n seq)
  (values (subseq seq 0 n) (subseq seq n)))

;;; rotate (from stackoverflow)
;;; only for the list form
(export '(fns::rotatel))
(defun rotatel (n l)
    (jscl::append-two (nthcdr n l) (butlast l (- (list-length l) n))))

(export '(fns::rotater))
(defun rotater (n l)
    (rotatel (- (list-length l) n) l))

;;; short forms for rotate left/right
(export '(fns::rotl))
(defun rotl (l) (rotatel 1 l))

(export '(fns::rotr))
(defun rotr (l) (rotater 1 l))


(in-package :cl-user)

;;;; EOF
