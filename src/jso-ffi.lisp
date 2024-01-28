;;; -*- mode:lisp; coding:utf-8 -*-

#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                    Copyright © 2017,2018,2023  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/moren-electron
            )     (                   2023, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL Moren edition 
      jgs   \__ __/                   Node.js
               ))
              //
             ((
              \)
|#

;;; experimental package for use in the Moren environment
;;; many features of the package will be further defined as compiler macros
;;; compiling the package as
;;;              (load "pathnames" :hook bin :output "ffi.js")
;;; 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ffi)
    (make-package :ffi :use (list 'cl))))

(in-package :ffi)

;;; internal utils
(defun %check-arg-string (s)
  (if (stringp s)
      (values t (jscl::oget s "length"))
      (values nil nil)))

;;; true if object is `js-object` not `cl-object's`
(export '(ffi::object-p))
(defun object-p (obj)
  (if (or (sequencep obj)
          (numberp obj)
          (symbolp obj)
          (functionp obj)
          (characterp obj)
          (packagep obj))
      nil
      t))

;;; Some short aliase's for internal coerced functions from  utils.lisp
(export '(ffi::list->[] ffi::[]->list ffi::js->cl ffi::cl->js))
(jscl::fset 'list->[] (fdefinition #'jscl::list-to-vector))
(jscl::fset '[]->list (fdefinition #'jscl::vector-to-list))
(jscl::fset 'js->cl (fdefinition #'jscl::js-to-lisp))
(jscl::fset 'cl->js (fdefinition #'jscl::lisp-to-js))



;;; true if obj eq js-null
(export '(ffi::js-null-p))
(defun js-null-p (obj) (%js-null-p obj))

;;; true if obj eq js-undefined
(export '(ffi::js-undefined-p))
(defun js-undefined-p (obj) (jscl::%js-undefined-p obj))

;;; internal hack to create objects in pure java-script.
;;; in most cases one should use something like:
;;;              (setf #j:nameOfsomething (lambda nil t)).
;;; but in some cases you need a pure java-script (performance).
;;; note:  don't use const/let and any other sugar.
;;;        jscl won't see it.
;;;        arrow functions are possible.
;;; use as (ffi::%be-evil "var BbB= {};")
(defmacro %be-evil (string)
  ;; be evil, use eval
  `(#j:eval ,string))

;;; temporary hack to create a promise instance, existing jscl::make-new
;;; doesn't do it correctly
(unless #j:make_Instance
  (let ((make-instance-proto
          "var make_Instance  =  function () {
                     var args = [].concat(null,Array.prototype.slice.call(arguments,2));
                     var fn = arguments[0][arguments[1]];
                     return new (Function.prototype.bind.apply(fn,args))();
                    };"))
    (#j:eval make-instance-proto)))

;;; from there may be use all Promise instance method, like this: then/catch/finally
(export '(ffi::make-promise))
(defun make-promise (fn)
    (#j:make_Instance #j:window "Promise" fn))

;;; Promise methods: all/allSettled/any/race/reject/resolve accessed by direct ref, as:
;;; #j:Promise:resolve etc...etc . define them to your own taste 
(export '(ffi::promise-then))
(defun promise-then (promise-instance ok &optional (nok nil nok-p))
  (if nok-p
      ((jscl::oget promise-instance "then") ok nok)
      ((jscl::oget promise-instance "then") ok)))


;;; define a function on a pure js. in most cases one should use something like:
;;;              (setf #j:nameOfsomething (lambda nil t)).
;;; but in some cases you need a pure java-script (performance).
;;;
;;; Example:
;;;   (ffi::def-js is-empty  is-empty "var isEmpty = (str) => (!str || 0 === str.length)")
;;; =>
;;;   (is-empty "")
;;;   T
;;;   (#j:isEmpty "")
;;;   T
;;; note:  don't use const/let and other sugar.
;;;        jscl won't see it. arrow functions are possible.
(export '(ffi::def-js))
(defmacro def-js (fn jsfn str)
  (unless (%check-arg-string str)
    (error "DEF-JS expected STRING, not this (~a)." str))
  ;; FN   - symbol under which the function will be available in Lisp
  ;; JSFN - the  equivalent to the Java script name of the function
  ;;        that is specified in the expression: "var js-name = function ..."
  ;;        i.e. lisp name `is-blank` eql js name `isBlank` (see function %NC below)
  (let ((sharp-name (%nc jsfn)))
    (#j:eval str)
    `(make_promise (lambda (resolve reject)
                     (funcall resolve
                              (jscl::fset ',fn (jscl::oget jscl::*root*  ,sharp-name)))))))

;;; def-obj-template
;;; create pure JS object from template, like this:
;;;   (defvar *o* (ffi:def-obj-template "{name: null, value: null}"))
(export '(ffi::def-obj-template))
#+nil
(defmacro def-obj-template (template)
  (let* ((id (symbol-name (gensym "obj")))
         (obj-string (jscl::concat "__JS_"  id "_CL =" template)))
    `(#j:eval ,obj-string)))

(defun %def-obj-template (template)
  (let* ((id (symbol-name (gensym "obj")))
         (obj-string (jscl::concat "__JS_"  id "_CL =" template)))
    (#j:eval obj-string)))

(jscl::fset 'def-obj-template #'%def-obj-template)

#+nil
(defmacro def-obj-template (template)
  (flet ((%m ()
           (print (list :temp template))
           (terpri)
           (let* ((id (symbol-name (gensym "obj")))
                  (obj-string (jscl::concat "__JS_"  id "_CL =" template)))
             (print obj-string)
             obj-string)))
    `(#j:eval ,(%m))))


;;; jscl::new
(export '(ffi::new))
(defun new (&rest ignore) (#j:Object))


;;; jscl::%js-vref
;;;    (ffi:ref "null")
;;;    => <JS-NULL>
;;;
;;;    (ffi:ref "undefined")
;;;    => <JS-UNDEFINED>
;;;
;;;    (funcall (ffi:ref "Array") 1 2 3)
;;;    => #(1 2 3)
(export '(ffi::ref))
(defmacro ref (var)
      `(jscl::%js-vref ,var))


;;; (winref "document")
;;;    => window.document
;;; (winref "screenX")
;;;    => nnn
(export '(ffi::winref))
(defmacro @winref (var)
  `(jscl::oget (jscl::%js-vref "window") ,var))


;;; or (ffi:ref "null")
;;; like this:
;;;     (defvar v-null (ffi:ref "null"))
;;;     (defvar v-null (ffi:js-null))
;;; but note:
;;;     (defparameter v-undefined (ffi:js-undefined))
;;;     (defparameter v-undefined (ffi:ref "undefined"))
;;;
(export '(ffi::js-null))
(defmacro js-null ()
  `(jscl::%js-vref "null"))

;;; or (ffi:ref "undefined")
(export '(ffi::js-undefined))
(defmacro js-undefined ()
  `(jscl::%js-vref "undefined"))

;;; Performance note
;;;
;;; Compatibility with common lisp is good idea, but  runtime  for` string` and `array`
;;; is to big, it not enough, to be used under high load.
;;;
;;; The following two macros ffi:|String| and ffi:|Array| provide access to the methods
;;; of the objects  `String` and `Array`.
;;;
;;; JSCL math functions are also slow, and not fully implemented.
;;; Macro ffi:|Math| provide access to the methods of the  `Math` object.
;;;

;;; ffi:|String|
;;;
;;;    (ffi:|String| "root.a.b.c" "split" ".")
;;;      => #(#<JS-OBJECT root> #\a #\b #\c)
;;;    (ffi::|String| "r.a.b.c" "split" ".")
;;;      => #(#\r #\a #\b #\c)
;;;  and so:
;;;    (map 'list 'jscl::js-to-lisp (ffi::|String| "r.a.b.c" "split" "."))
;;;      => ("r" "a" "b" "c")
;;;
;;; Hack `jscl` string:
;;;   (setq s "abcd")
;;;   (ffi:obj-keys s) => ("0" "1" "2" "3")
;;;   (ffi::get-own-prop-names s) => ("0" "1" "2" "3" "length")
;;;   (ffi:getprop s "2") => "c"
;;;   (ffi:setprop (s "stringp") nil)
;;;   s => #(#\a #\b #\c #\d)
(export '(ffi::|String|))
(defmacro |String| (s  m  &body a)
  ;; `s` must be a `string`
  ;; `m` must be a `string`
  `(let ((o (jscl::lisp-to-js ,s)))
     ((jscl::oget o ,m) ,@a)))

;;; Array instance methods
;;;
;;; ffi:|Array|
;;;    (setq *a (funcall (ffi:ref "Array") 1 2 3))
;;;    (ffi:|Array| *a "push" 4)
;;;         *a => #(1 2 3 4)
;;;    (ffi:|Array| *a "splice" 2)
;;;         *a => #(1 2)
;;;
;;; something like this:
;;;          (defun array-splice (a from) (ffi:|Array| a "splice" from))
;;;          (defun to-array (a v) (ffi:|Array| a "push" v))
(export '(ffi::|Array|))
(defmacro |Array| (obj method &body args)
  ;; `obj` must be a `vector`
  ;; `method` must be a `string`
  `((jscl::oget ,obj ,method) ,@args))

;;; Array static method
;;; Creates a new Array instance from an iterable or array-like object
;;;  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from#syntax
;;;  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from#examples
(export '(ffi::array-from))
(defun Array-from (obj &optional (mapfn map-p nil) env)
  (if map-p
      (#j:Array:from obj mapfn)
      (#j:Array:from obj)))

;;; ffi:|Math|
;;;  (ffi:|Math| "log" 2)
;;;     => 0.6931471805599453
;;;  (ffi:|Math| "sin" 3)
;;;     => 0.1411200080598672
;;;  (ffi:|Math| "clz32" 1)
;;;     => 31
(defvar *math-lib* (ref "Math"))

(export '(ffi::|Math|))
(defmacro |Math| (method &body args)
  ;; `method` must be a `string`
  `((jscl::oget *math-lib* ,method) ,@args ))


;;; RegExp constructor
;;; flags may contain any combination of the following characters:
;;; 
;;;   d (indices) Generate indices for substring matches.
;;;   g (global) Find all matches rather than stopping after the first match.
;;;   i (ignore case) When matching, casing differences are ignored.
;;;   m (multiline) Treat beginning and end assertions (^ and $) as working over multiple lines.
;;;     In other words, match the beginning or end of each line (delimited by \n or \r),
;;;     not only the very beginning or end of the whole input string.
;;;   s (dotAll) Allows . to match newlines.
;;;   u (unicode) Treat pattern as a sequence of Unicode code points.
;;;   y (sticky) Matches only from the index indicated by the lastIndex property of this regular
;;;     expression in the target string. Does not attempt to match from any later indexes.

#|          something like this:

(defvar *kebab-reg* (#j:RegExp "[A-Z]" "g"))
(defun kebab (name)
  (flet ((%js (s) (jscl::lisp-to-js s)))
    ((jscl::oget (%js name) "replace")
     *kebab-reg*
     (lambda (m &rest any)
       (jscl::concat "-" ((jscl::oget (%js m) "toLowerCase")) ) ))))
|#
(export '(ffi::regexp))
(defun regexp (pattern &optional (flag "" f-op))
  (check-type pattern string "FFI:regexp")
  (unless (%check-arg-string flag)
    (error "FFI:regexp expected STRING FLAG, not this (~a)." flag))
  (#j:RegExp pattern flag))


#|

(setq e (regexp "t(e)(st(\\d?))" "g"))
>> #<JS-OBJECT /t(e)(st(\d?))/g>

(#j:Array:from (|String| "test1test2" "matchAll" e))
>> #(#(#<JS-OBJECT test1> #\e #<JS-OBJECT st1> #\1) #(#<JS-OBJECT test2> #\e #<JS-OBJECT st2> #\2))

(#j:Array:from (|String| "test1test2" "matchAll" (regexp "t(e)(st(\\d?))" "g")))
>> #(#(#<JS-OBJECT test1> #\e #<JS-OBJECT st1> #\1) #(#<JS-OBJECT test2> #\e #<JS-OBJECT st2> #\2))

(map 'list (lambda (x)
             (list (ffi:js->cl (first x))
                   (ffi:js->cl (second x))
                   (ffi:js->cl (third x))
                   (fourth x  )))
     (map 'list #'ffi:[]->list
          (#j:Array:from (ffi:|String| "test1test2" "matchAll" e))))
|#

;;; name convertor from symbol to string
;;;
;;; symbol := (%nc :a) => "A"
;;;           (%nc 'a) => "a"
;;;           (%nc '-a) => "A"
;;;           (%nc '---a => "A"
;;;           (%nc 'a-a) => "aA"
;;; (%nc :|a-b|) => "aB"
;;; (%nc :|a -b| => "a_B")
;;; string:= (%nc "AbC" => "AbC")
;;; note: about legal name form
;;;       :|@name of subject| -> js:'@name_of_subject'
;;;                              so, (ffi:getprop obj :|@name of subject|) => value-of
;;;                              but js: rds['@name_of_subject'], not rds.@name_of_subject
;;;
(defun %nc (name)
  (cond ((stringp name) (return-from %nc name))
        ((symbolp name) (setq name (string-downcase (symbol-name name))))
        (t (error "FFI: Bad object property name: ~a." name)))
  (prog ((len)
         (upcase nil)
         (ch)
         (idx 0)
         (result))
     (setq len (length name))
   rdr
     (if (= idx len) (go eol))
     (setq ch (char name idx)
           idx (1+ idx))
     (if (char= #\- ch)
         (progn
           (setq upcase t)
           (go rdr)))
     (if (char= #\space ch) (setq ch #\_))
     (push
      (if upcase
          (progn
            (setq upcase nil)
            (char-upcase ch))
          ch)
      result)
     (go rdr)
   eol
     (if (null result) (error "FFI: Bad syntax ~a for object key." name))
     (setq result (apply 'jscl::concat (reverse result)))
   Exit
     (return result)))

;;; make JS object without Object definitions
;;; (make-obj "a" 1 "b" 2 "c" (make-obj "f" (lambda nil t)))
;;; => {a: 1, b:2, c: {f: <fn>}}
(export '(ffi::make-obj))
(defun make-obj (&rest kv)
  (let ((obj (#j:Object))
        (idx 0)
        (key-val))
    (if (oddp (list-length kv))
        (error "MAKE-OBJ: Too few arguments ~a." kv))
    (dolist (it kv)
      (if (oddp idx)
          (setf (jscl::oget obj (%nc key-val)) it)
          (setq key-val it))
      (incf idx))
    obj))

;;; New object: from list of pairs: ((name value) ... (name value))
;;;             from vector form from FFI:ENTRIES
(export '(ffi::obj-from-entries))
(defun obj-from-entries (pairs)
  (#j:Object:fromEntries
   (cond ((vectorp pairs) pairs)
         ((consp pairs)
          (map 'vector (lambda (x)
                         (cond ((consp x)
                                (jscl::list-to-vector x))
                               ((vectorp x) x)
                               (t (error "FFI: wrong form for entries ~a." x))))
               pairs))
         (t (error "FFI: wrong argument for entries ~a." pairs)))))

;;; return vector of vectors: (name value)
;;; note: may be exception if object keys has length 1 or 2 symbols
;;;       this is a birth trauma jscl associated with
;;;       the conversion of a string from js to cl
(export '(ffi::entries))
(defun entries (obj)
  (#j:Object:entries obj))

;;; js object iterator
;;; (ffi:for-each obj  #'(lambda (key val) (print val)) )
;;; "aaa"
;;; "bbb"
;;; nil
(export '(ffi::for-each))
;;; todo: note: deprecate
(defun for-each (jso fn)
  ;; fn => (lambda (key val) ...)
  (jscl::%lmapcar
   (lambda (key)(funcall fn key (jscl::oget jso key)))
   (jscl::%lmapcar #'jscl::js-to-lisp
                   (jscl::vector-to-list (#j:Object:keys jso)))))

;;; todo: add observer / observer funcall
(defun for (jso)
  ((jscl::oget (#j:Object:keys jso) "forEach")
   (lambda (key &rest ignore) (print (list key (jscl::oget jso key))))))

;;; todo: note: deprecate
(export '(ffi::mapobj))
(defun mapobj (object observer)
  ;; observer => (lambda (key val) ...)
  (let ((wrap (lambda (key other ignore) (funcall observer key (jscl::oget object key)))))
    ((jscl::oget (#j:Object:keys object) "forEach")
     wrap)
    nil))


;;; Return object keys, only enumerable props!!!
;;; ie obj <- (ffi:new)
;;;    (ffi:defprop obj "name" :value "Meister" :writable t) 
;;;    (ffi:obj-keys obj) -> nil
;;;
(export '(ffi::obj-keys))
(defun obj-keys (jso)
  (jscl::%lmapcar #'jscl::js-to-lisp (jscl::vector-to-list (#j:Object:keys jso))))

(export '(ffi::get-own-prop-names))
(defun get-own-prop-names (obj)
  ;; all keys
  (jscl::%lmapcar #'jscl::js-to-lisp (jscl::vector-to-list (#j:Object:getOwnPropertyNames obj))))

;;; true  if the specified object has the indicated property as its own property
(export '(ffi::has-own))
(defun has-own (obj name)
  ;; name m.b. STRING
  (#j:Object:hasOwn obj name))

;;; js-object to list
;;; => (("aa" 1) ("bb" 2))
;;; JS NULL marked as NIL (by default)
(export '(ffi::obj-list))
(defun obj-list (jso &optional (null-stamp nil))
  (let ((raw))
    (jscl::%lmapcar (lambda (k)
                      (setq raw (jscl::oget jso k))
                      (if (jscl::js-null-p raw)(setq raw null-stamp))
                      (list k raw))
                    (jscl::%lmapcar #'jscl::js-to-lisp
                                    (jscl::vector-to-list (#j:Object:keys jso))))))

;;; Merge few objects to new object
;;; The identical properties are replaced
;;; See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign
;;; for details
;;;      (ffi:obj-merge (ffi:new) obj1 obj2)
;;;      (ffi:obj-merge obj1 obj2 obj3)
(export '(ffi::obj-merge))
(defun obj-merge (&rest object)
  (apply #j:Object:assign (jscl::new) object))

;;; delete properties from obj
(export '(ffi::delete-prop))
(defun delete-prop (object prop)
  (jscl::delete-property prop object))

;;; freezing an object prevents extensions and makes existing
;;; properties non-writable and non-configurable. 
(export '(ffi::freeze))
(defun freeze (obj)
  (#j:Object:freeze obj))

;;; determines if an object is frozen
(export '(ffi::is-frozen))
(defun is-frozen (obj)
  (#j:Object:isFrozen obj))

;;; Seals an object.
(export '(ffi::seal))
(defun seal (obj)
  (#j:Object:seal obj))

(export '(ffi::is-sealed))
(defun is-sealed (obj)
  (#j:Object:isSealed obj))

;;; determines if an object is extensible (whether it can have
;;; new properties added to it)
(export '(ffi::is-extensible))
(defun is-extensible (obj)
  (#j:Object:isExtensible obj))

;;; js object method bind args to current environment and call
(export '(ffi::bind-call))
(defmacro bind-call ((obj &rest methods) &body args)
  `(funcall ((jscl::oget ,obj ,@methods "bind") ,obj ,@args)))

(export '(ffi::bind))
(defmacro bind ((obj &rest methods) &body args)
  `((jscl::oget ,obj ,@methods "bind") ,obj ,@args))

;;; js object method call without binding
;;; a<- {a:0, fn: (lambda (x) (ffi:setprop (jscl::this "a"))}
;;; (ffi:call (a "fn"))
(export '(ffi::call))
(defmacro call ((obj &rest methods) &body args)
  `((jscl::oget ,obj ,@methods) ,@args))


;;; js object get/set macro

;;; (ffi:getprop obj "aaa" "bbb" "ccc")
;;; => (jscl::oget obj "aaa" "bbb" "ccc")
(export '(ffi::getprop))
(defmacro getprop (obj &rest pathes)
  ;; obj - js object
  ;; pathes - string | string*
  `(jscl::oget ,obj ,@pathes ))

;;; (ffi::setprop (obj "aaa" ) (new))
;;;   => (setf (oget obj "aaa") (new))
;;; (ffi:setprop (obj "aaa" "bbb") (new))
;;; (ffi::setprop (obj "aaa" "bbb" "ccc") "place")
;;;   obj => {aaa: {bbb: {ccc: "place"}}}
(export '(ffi::setprop))
(defmacro setprop ((obj &rest pathes) &body value)
  ;; obj - js object
  ;; pathes - string | string*
  `(setf (jscl::oget ,obj ,@pathes) ,@value))


;;; define js object property
;;; details see at:
;;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty
(export '(ffi::defprop))

(defun defprop (object prop &key value #+nil get #+nil set writable enumerable configurable)
    (let ((args)
          (prop-name
            (cond ((listp prop) (setq prop-name (jscl::%lmapcar '%nc prop)))
                  (t (setq prop-name (%nc prop))))))
        (if value (push (list "value" value) args))
        (if writable (push (list "writable" writable) args))
        (if enumerable (push (list "enumerable" enumerable) args))
        (if configurable (push (list "configuarble" configurable) args))
        #+nil (if get (push (list "get" get) args))
        #+nil (if set (push (list "set" set) args))
        (setq args (apply 'make-obj (apply 'append args)))
        (cond ((listp prop-name)
               (let ((arguments
                       (append prop-name (list args))))
                   (apply #j:Object:defineProperty object arguments)))
              (t (#j:Object:defineProperty object prop-name args))))
  object)

;;; Get own property descriptor
;;; REPL =>
;;;   (setq obj (funcall (ffi:ref "Object")))
;;;   (ffi:defprop obj "name" :value "Meister" :writable t)
;;;   (ffi:get-prop-descr obj)
;;;   => #<JS-OBJECT [object Object]>
;;;   (ffi:to-list *)
;;;    => (("value" "Meister") ("writable" T) ("enumerable" NIL) ("configurable" NIL))
;;;
(export '(ffi::get-prop-descr))
(defun get-prop-descr (object prop-name)
  ;; object - js object
  ;; prop-name - string | keyword | symbol
  (#j:Object:getOwnPropertyDescriptor object (%nc prop-name)))

;;; JS THIS for lisp let form
;;; (lambda (n)
;;;  (ffi:with-this self
;;;     (ffi:setprop (self "name") "Meister"))))
(export '(ffi::with-this))
(defmacro with-this (name &rest body)
    `(let ((,name jscl::this))
         ,@body))
#|
   Object inheritance (prototype)

   (defvar stub-o (ffi:make-obj
                 "name" nil
                 :|weight| nil
                 "setWeight" (lambda (x) (ffi:with-this self (ffi:setprop (self "weight") x)))
                 "setName" (lambda (x) (ffi:with-this self (ffi:setprop (self "name") x)))))

   ;;; constructor
   (defun make-o1 (name weight)
      (let ((o (ffi:new)))
         (ffi:setprop (o "__proto__") stub-o)
         (ffi:setprop (o "show")
            (lambda ()
              (ffi:with-this self
                 (format t "~&1: Object ~a with weigth ~a~&"
                    (ffi:getprop self "name")
                    (ffi:getprop self "weight")))))
         (ffi:defprop o "showOther" :enumerable nil
              :value (lambda ()
                       (ffi:with-this self
                         (format t "~&2: Object ~a with weigth ~a~&"
                                    (ffi:getprop self "name")
                                    (ffi:getprop self "weight")))))
         (ffi:call (o "setName") name)
         (ffi:call (o "setWeight") weight)
         (ffi:call (o "show"))
         (ffi:call (o "showOther"))
   o))

   
   (setq ro (make-o1 "aaa" 113))
   =>
   1: Object aaaa with weigth 113
   2: Object aaaa with weigth 113
   #<JS-OBJECT [object Object]>

   (ffi:obj-list oo)
   =>
   (("show" #<FUNCTION>) ("name" "aaaa") ("weight" 113))

|#



#|
                   Deprecated, see commentary abowe. For more complex cases, use native JS


;;; (:constructor (arg arg arg) &body)
(defun %do-constructor-clause (tail)
    `(lambda (,@(car tail)) ,@(cdr tail)))

;;; (:method method-name (args list) &body)
(defun %do-method-clause (tail)
    (let ((code (cdr tail)))
        (values (car tail)
                `(lambda ,(car code) ,@(cdr code)))))

;;; (:prop name &optional value)
(defun %do-prop-clause (tail)
    (values (car tail) (cdr tail)))


;;; JS THIS for lisp let form
;;; (lambda (n)
;;;  (ffi:with-this self
;;;     (ffi:setprop (self "name") "Meister"))))
(export '(ffi::with-this))
(defmacro with-this (name &rest body)
    `(let ((,name jscl::this))
         ,@body))

;;; setup object prototype method
(defun %set-proto-method (proto item)
    `(defprop ,proto ,(%nc (car `,item)) :value ,(cadr `,item)))

;;; setup object propertie
;;; (:prop temperature :writable t :enumerable t :value 36.6)
;;; (:prop temperature)
(defun %set-props (proto item)
    (let ((name)
          (descr))
        (unless (setq name (car item))
            (error "DEFOBJECT: Prop name must be"))
        (setq descr (cadr item))
        (if descr (push 'list descr))
        (if descr
            `(apply 'defprop ,proto ,(%nc `,name) ,descr)
            `(defprop ,proto `,(%nc `,name)))))


;;; macro defobject
;;;
;;; make native js object
;;;
;;; name::= string  JS object name => #j:xxx
;;; inherit::= keyword pair => :inherit #j:ParentObject
;;; clauses::= :prop | :constructor | :method
;;;    (:prop weight
;;;           :value 0 :writible t :enumerable t :configurable t
;;;           get (lambda () (with-this (self)
;;;                  (call-meth (self "get-weight")))
;;;           set (lambda () (with-this (self)
;;;                  (call-meth (self "get-weight"))))
;;;    ie Object.defineProperties() parameters see
;;;    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty
;;;    (:constructor (name &optional (weight 0) (height 0))
;;;        (with-this (self)
;;;            (setprop (self "name") name)
;;;            (setprop (self "weight") weight)
;;;            (setprop (self "height") height)) ;;  Object.prototype.constructor
;;;    (:method set-weight (s) (setprop (jscl::this "weight") s))

|#

#|

(export '(ffi::defobject))
(defmacro defobject ((name &key inherit) &rest clauses)
    (let ((parent)
          (inherited-code)
          (constructor)
          (own-construct-code)
          (constructor-code)
          (methods)
          (props)
          (owns)
          (tmp))
        (if inherit (setq parent inherit))
        (dolist (it clauses)
            (case (car it)
              (:prop (multiple-value-bind (name code) (%do-prop-clause (cdr it))
                         (push (list name code) props)))
              (:constructor (setq constructor (%do-constructor-clause (cdr it))))
              (:method (multiple-value-bind (name code) (%do-method-clause (cdr it))
                           (push (list name code) methods)))
              (otherwise (error "DEFOBJECT: unknow clause ~a." (car it)))))
        (setq owns (append name (list "prototype")))
        (if constructor
            (setq constructor-code `((setf ,name ,constructor)))
            (progn
                (setq tmp owns)
                (setq owns (append name (list "__proto__")))
                ;; create and add protopype
                (setq constructor-code `((setf ,name (#j:Object:create (jscl::new)))
                                         (setf ,tmp ,owns)) )))

        (if parent
            (setq inherited-code
                  (let ((parent-prototype (append parent (list "prototype")))
                        (owns-proto-constructor (append owns (list "constructor"))))
                      `((setf ,owns (#j:Object:create ,parent-prototype))
                        (setf ,owns-proto-constructor ,name)))))
        `(progn
             ,@constructor-code
             ,@inherited-code
             ,@(jscl::%lmapcar (lambda (item) (%set-proto-method `,owns `,item)) `,methods)
             ,@(jscl::%lmapcar (lambda (it) (%set-props `,owns `,it)) `,props) )))

|#

(push :ffi *features*)
(in-package :cl-user)

;;; EOF
