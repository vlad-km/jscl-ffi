;;; -*- mode:lisp;  coding:utf-8 -*-
#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                    Copyright © 2017,2018,2023  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/moren-electron
            )     (                   2023, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL >= version 0.8.2  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#

;;; This must be at boot phase
;;; using an outdated version rxjs 5.*

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :rx)
    (make-package :rx :use (list 'cl)))
  (unless #j:Rx
    (setf #j:Rx (require "./rxjs.umd.min"))))

(in-package :rx)

(defvar *moren-rx-subjects* (jscl::new))

(export '(rx::listen))
(defun listen (name listener)
    (let ((subj (jscl::make-new #j:Rx:Subject)))
        (setf (jscl::oget *moren-rx-subjects* (jscl::concat "$" (string name))) subj)
        ((jscl::oget subj "subscribe") listener)) )

(export '(rx::emit))
(defun emit (name arg)
    (let ((subj))
        (setq subj (jscl::oget *moren-rx-subjects* (jscl::concat "$" (string name))))
        ((jscl::oget subj "next") arg))  )

(push :rx *features*)

(in-package :cl-user)

;;; EOF
