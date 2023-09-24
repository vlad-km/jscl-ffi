;;; -*- mode:lisp; coding:utf-8 -*-

#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                                                                      @vlad-km
            /     \                   
            )     (                   Copyright © 2018,2023  @vlad-km
           /       \                  Redesign for Electron >= electron@21.2.2
           \       /                               JSCL >= version 0.8.2  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#



(eval-when (:compile-top-level :load-toplevel :execute)
  (unless (find-package "CSS")
    (defpackage :css
      (:use :cl)
      (:export #:sheets-length
               #:style-sheet-info
               #:style-sheet-observe
               #:display-css-sheets
               #:display-sheet-table
               #:create-style-sheet
               #:add-css-rule
               #:delete-css-rule
               #:inline
               #:le-css))))

(in-package :css)

(export '(jscl::js-null-p jscl::concat))

;;; local macro for JSO

(defmacro jso_mcall ((obj &rest methods) &body body)
  `(funcall ((jscl::oget ,obj ,@methods "bind") ,obj ,@body)))

(defmacro jso_call ((obj &rest methods) &body body)
  `((jscl::oget ,obj ,@methods) ,@body))

(defmacro jso_get ((obj &rest methods))
  `(jscl::oget ,obj ,@methods ))

(defmacro jso_set ((obj &rest methods) &body body)
  `(setf (jscl::oget ,obj ,@methods) ,@body))


;;; css
;;; options
;;;{
;;;    helper: "ui-resizable-helper",
;;;    grid: [10, 10]
;;;}


;;;; CSS SheetTables

;;; length all Sheets
(defun sheets-length ()
  (jso_get (#j:document:styleSheets "length")))


;;; style sheet info
(defun style-sheet-info (idx)
  (let* ((obj (jso_get (#j:document:styleSheets (string idx))))
         (type (jso_get (obj "type")))
         (href (jso_get (obj "href")))
         (length (jso_get (obj "rules" "length"))))
    (flet ((prevent-null (x) (if (jscl::js-null-p x) nil x)))
      (values (prevent-null type) (prevent-null href) (prevent-null length)))))

;;; style sheet content
(defun style-sheet-observe (idx)
  (let* ((sheet (jso_get (#j:document:styleSheets (string idx))))
         (rules (jso_get (sheet "rules"))))
    (map 'list (lambda (obj)
                 (let ((selector (jso_get (obj "selectorText")))
                       (css (jso_get (obj "cssText"))))
                   (list selector css)))
         rules)))

;;; print short info about Sheets
(defun display-css-sheets ()
  (flet ((print* (&rest it)
           (dolist (item it) (princ item) (princ " ")) (terpri) ))
    (dotimes (idx (sheets-length))
      (multiple-value-bind (type href length) (style-sheet-info idx)
        (print* (jscl::concat idx ": ") type href length)))))


;;; print style sheet content
(defun display-sheet-table (idx)
  (let ((pos 0))
    (dolist (it (style-sheet-observe idx))
      (princ pos)
      (princ ": ")
      (princ (cadr it))
      (terpri)
      (incf pos))))

;;; find sheet table by href
(defun find-sheet-by-href (pat)
  (dotimes (idx (sheets-length))
    (multiple-value-bind (type href length) (style-sheet-info idx)
      (when
          (>= (jso_call ((jscl::lisp-to-js href) "indexOf") pat ) 0)
        (return-from find-sheet-by-href idx))))
  nil)


;;; Create new CSSSheet at document.head
;;; return CSSSheetObj
;;;        (1- length-CSSSheets)
;;;
;;; For adding rules use:
;;;    (add-css-rule num rule-string) where num is (integer 0 length-CSSSheets))
;;;    (add-css-rule CSSSheetObj rule-string)
;;;
(defun create-style-sheet (rules &optional (idx 0))
  (let* ((elt (jso_mcall (#j:document:head "appendChild")
                         (#j:window:document:createElement "style")))
         (sheet (jso_get (elt "sheet"))))
    (jso_mcall (sheet "insertRule") rules idx)
    (values sheet (1- (sheets-length)))))

(defun add-css-rule (selector rule &optional (idx 0))
  (cond ((integerp selector)
         ;; insert rule by Sheets[number]
         (let* ((sheet (jso_get (#j:document:styleSheets (string selector))))
                ;;(sheet (jso_get (elt "cssRules")))
                (idx (jscl::oget sheet "cssRules" "length")))
           ;;(print (list :css-num selector sheet idx))
           ;;(#j:console:log "SHEET" sheet)
           (jso_mcall (sheet "insertRule") rule idx)))
        ((jscl::js-object-p selector)
         ;; insert rule throug CSSSheetObj from (create-style-sheet...)
         (let* ((sheet selector)
                (idx (jscl::oget sheet "cssRules" "length")))
           (jso_mcall (sheet "insertRule") rule idx)))
        (t (error "WTF ~a?" selector))))

;;; delete rule from set
(defun delete-css-rule (&key sheet idx)
  (when (or (null sheet) (null idx))
    (error "Keys :sheet & :idx must be"))
  (let ((faddr (jso_get (#j:document:styleSheets (string sheet)))))
    (jso_mcall (faddr "deleteRule") idx)))

;;; create css rule
;;;
;;;
;;; (le-css rname items*)
;;;
;;; for #name  name::mode
;;; use "#name" "name::after"
;;;
;;; for create-style-sheet
;;;     (le-css .name :color "black")
;;;     => ".name {color:black}"
;;;     or
;;;;    (concat (le-css ...) " " (le-css ...) )
;;;
;;; for add-css-rule
;;;     (le-css nil :color "black" :background-color "red")
;;;     => "color:black;background-color:red;"
;;;

;;; (css:inline `(:color "black" :background-color "red"))
;;; (defvar *e '(:background-color |Yellow| :color #x666))
;;; (css-inline `(:font-weight normal ,@*e))
(defun inline (rule)
  (lessi nil rule))

#+nil
(defmacro le-css (l-rule &rest r-rule)
    (let ((rname (cond  ((stringp l-rule) l-rule)
                        ((symbolp l-rule) `',l-rule))))
        `(lessi ,rname ',r-rule)))

(defun le-css (rules)
  (apply 'jscl::concat
         (loop for rule in rules
               collect (lessi (car rule) (cdr rule)))))

#+nil
(defun lessi (left right)
    (when (oddp (length right)) (error "Lessi: odd length arguments"))
    (let ((pairs nil))
        (labels ((store (var val) (push (jscl::concat var val) pairs))
                 (lower (x) (jso:call ((jscl::lisp-to-js x) "toLowerCase")))
                 (change-name (key) (lower (jscl::concat (symbol-name key) ":")))
                 (conv (var val)
                     (store (change-name var) (jscl::concat val ";"))))
            (tagbody parser
             rdr
               (setq vname (pop right))
               (unless vname (go done))
               (setq value (pop right))
               (conv vname value)
               (go rdr)
             done
               (if left (push "} " pairs)))
            (setq pairs (reverse pairs))
            (when left
                (push " { " pairs)
                (push (cond ((stringp left) left)
                            (t (lower (symbol-name left))))
                      pairs))
            (apply 'jscl::concat pairs ))))


(defun lessi (left right)
  (when (oddp (length right)) (error "Lessi: odd length arguments"))
  (let ((pairs nil))
    (labels ((store (var val) (push (jscl::concat var val) pairs))
             (lower (x) (jso_call ((jscl::lisp-to-js x) "toLowerCase")))
             (change-name (key) (lower (jscl::concat (symbol-name key) ":")))
             (change-val (val) (if (symbolp val) (lower (symbol-name val)) val))
             (conv (var val)
               (store (change-name var) (jscl::concat (change-val val) "; "))))
      (tagbody parser
       rdr
         (setq vname (pop right))
         (unless vname (go done))
         (setq value (pop right))
         (conv vname value)
         (go rdr)
       done
         (if left (push "} " pairs)))
      (setq pairs (reverse pairs))
      (when left
        (push " { " pairs)
        (push (cond ((stringp left) left)
                    (t (lower (symbol-name left))))
              pairs))
      (apply 'jscl::concat pairs ))))

(in-package :cl-user)


;;; EOF
