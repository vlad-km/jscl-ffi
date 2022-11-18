(intern "|null|" :keyword)

(defun {null} (x &key (subst nil subst-p) 
                 &aux (s1 (not subst-p)))
  (if (jscl::js-null-p x)
      (if s1
          :|null|
        subst)
    x))

;;; (map 'list  (lambda (x) ({null} x :subst :null)) (#j:Object:values #j:O))
;;; (map 'list  #'{null} (#j:Object:values #j:O))


(defun {v} (o)
  (mapcar #'{null}
          (map 'list #'jscl::js-to-lisp (#j:Object:values o))))

(defmacro |array|((obj &rest methods) &body args)
  `((jscl::oget ,obj ,@methods) ,@args))

(defmacro |array|(obj method &body args)
  `((jscl::oget ,obj ,method) ,@args))


(defmacro |string|((obj &rest methods) &body args)
  `(let ((o (jscl::lisp-to-js ,obj)))
     ((jscl::oget o ,@methods) ,@args)))

(defmacro |string| (s  m  &body a)
  `(let ((o (jscl::lisp-to-js ,s)))
     ((jscl::oget o ,m) ,@a)))



(defmacro {f} ((obj &rest methods) &body args)
  `((jscl::oget ,obj ,@methods) ,@args))


(defmacro |string| (s  m  &body a)
  `(let ((o (jscl::lisp-to-js ,s)))
     ((jscl::oget o ,m) ,@a)))


(defun padding (n)
  (|string| " " "repeat" n))

(defun padding-left (o n)
  (concatenate 'string
               (|string| " " "repeat" n)
               o))

#|
(defun left-Fill-Num (num targetLength) 
 (|string| (|string| num "toString") "padStart" targetLength 0))

(left-fill-num 123 5)
>> "00123"

CL-USER> (|string| "hey JudE" "search" (regexp "[A-Z]"))
4
CL-USER> (|string| "hey JudE" "search" (regexp "[.]"))
-1

CL-USER> (|string| "??" "codePointAt" 1) 
56845

CL-USER> (|string| "??" "codePointAt" 0)
128525

|#



(defun |RegExp| (p &optional (f nil f-p))
  (if (not f-p)
      (#j:RegExp p)
    (#j:RegExp p f)))


(defmacro |array|(obj method &body args)
  `((jscl::oget ,obj ,method) ,@args))

#|

(setq e (regexp "t(e)(st(\\d?))" "g"))
>> #<JS-OBJECT /t(e)(st(\d?))/g>

(#j:Array:from (|string| "test1test2" "matchAll" e))
>> #(#(#<JS-OBJECT test1> #\e #<JS-OBJECT st1> #\1) #(#<JS-OBJECT test2> #\e #<JS-OBJECT st2> #\2))

(#j:Array:from (|string| "test1test2" "matchAll" (regexp "t(e)(st(\\d?))" "g")))
>> #(#(#<JS-OBJECT test1> #\e #<JS-OBJECT st1> #\1) #(#<JS-OBJECT test2> #\e #<JS-OBJECT st2> #\2))

|#
