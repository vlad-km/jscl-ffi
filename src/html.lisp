;;; -*- mode:lisp;  coding:utf-8 -*-
#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                    Copyright В© 2017,2018,2023  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/moren-electron
            )     (                   2023, Code redesign, added features
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL Moren edition
      jgs   \__ __/
               ))
              //
             ((
              \)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "HTML")
    (defpackage :html
      (:use :cl)
      (:export #:document-title    #:document-head #:document-body
               #:element-value
               #:dom-element-p 
               #:get-element-by-id #:get-element-by-class
               #:has-attribute     #:set-attribute      #:get-attribute #:remove-attribute
               #:set-event         #:add-event-listener #:rem-event-listener
               #:node-append-child #:remove-child
               #:mount             #:umount
               #:get-class-name    #:get-class-list  #:contains-class-p
               #:add-class-name    #:remove-class-name
               #:toggle-class-name
               #:parent-node       #:node-name
               #:has-childs        #:child-count    #:get-childs
               #:first-child       #:last-child     #:next-sibling
               #:insert-before     #:insert-after   #:insert-top
               #:element-width     #:element-height #:page-width #:page-height
               #:visible-width     #:visible-height
               #:create-element
               #:declare-element
               ))))

(in-package :html)

;;; DOM accessor's

;;; get/set window.document.title
(defun document-title (&optional e)
  (cond ((null e)
         (let ((title #j:window:document:title))
           (if (zerop title)
               (return-from document-title nil)
               title)))
        ((stringp e)
         (setf (jscl::oget #j:window:document "title") e))
        (t nil)))

;;; document head & body
(defun document-body ()
  #j:window:document:body)

(defun document-head ()
  #j:window:document:head)

;;; get/set el.value
(defun element-value (el &optional val)
  (cond ((null val)(jscl::oget el "value"))
        (t (setf (jscl::oget el "value") val))))

;;; true if element is DOM element
;;; hmmm... highly likely
(defun dom-element-p (u)
  (if (jscl::oget u "appendChild")  t nil))

;;; get element by ID
(defun get-element-by-id (id)
  (#j:window:document:getElementById id))

;;; get element by class-name
(defun get-elements-by-class (name)
  (#j:window:document:getElementsByClassName name))

;;; true if element has attribute 
(defun has-attribute (elem name)
  ((jscl::oget elem "hasAttribute") name))

;;; set element attribute
(defun set-attribute (elm attr val)
  (funcall ((jscl::oget elm "setAttribute" "bind")  elm attr val)))

;;; get-attribute
;;; return string or <js-null>
#+nil
(defun get-attribute (elm attr)
  (funcall ((jscl::oget elm "getAttribute" "bind") elm attr)))

(defun get-attribute (elm attr)
  ((jscl::oget elm "getAttribute") attr))


;;; remove attribute
(defun remove-attribute (elm attr)
  (funcall ((jscl::oget elm "removeAttribute" "bind") elm attr)))

;;; dataset get/set
;;; if dataset.key is absent returned `nil`
;;; so, for setting value=nil, use something like this  :true/:false
(export '(html::element-data))
(defun element-data (el key &optional (value nil val-p))
  (if val-p
      (setf (jscl::oget el "dataset" key) value)
      (jscl::oget el "dataset" key)))

;;; element TAG
;;; return localName tagName nodeName
;;;
;;; (element-tag (html:span))
;;; =>
;;;     "span"
;;;     "SPAN"
;;;     "SPAN"
(export '(html::element-tag))
(defun element-tag (el)
  (values 
   (jscl::oget el "localName")
   (jscl::oget el "tagName")
   (jscl::oget el "nodeName")))

;;; HTML element predicate
;;; (html-p (jscl::new))
;;; => nil
;;; (html-p (html:span))
;;; => t
(export '(html::html-p))
(defun html-p (obj)
  (and (jscl::oget obj "tagName")
       (= ((jscl::oget (jscl::lisp-to-js (string obj)) "indexOf") "HTML") 8 )))

;;; innerHtml getter/setter
(export '(html::inner-html))
(defun inner-html (el &optional (value nil val-p))
  (if val-p
      (jscl::oget el "innerHtml")
      (setf (jscl::oget el "innerHTML") value)))

;;; element hidden
;;; getter / setter
(export '(html::element-hidden))
(defun element-hidden (el &optional (value nil val-p))
  (if val-p
      (setf (jscl::oget el "hidden") value)
      (jscl::oget el "hidden")))

;;; events

;;; (set-event p "click" #'click-dispatch)
;;;     => ok
;;; (set-event p "click" (lambda (x) x))
;;;     => no good
;;;        this handler will not be removed
;;;     at this case, well be do next:
;;;        (setq handler #'(lambda nil nil))
;;;        (setf (gethash 'tag handlers-cash) handler)
;;;        (set-event p "click" handler)
;;;        (rem-event-listener r (gethash 'tag handlers-cash))
;;;     something like this
;;;
(defun set-event (elm event function)
  (setf (jscl::oget elm event) function))

(defun add-event-listener (el event handler)
  (funcall ((jscl::oget el "addEventListener" "bind") el event handler)))

(defun rem-event-listener (el event handler)
  (funcall ((jscl::oget el "removeEventListener" "bind") el event handler)))

;;; append's family

;;; child may be a (list*)
(defun node-append-child (child parent)
  (flet ((%make-text-node (it)
           (#j:window:document:createTextNode it))
         (%append (e)
           ((jscl::oget parent "appendChild") e)))
    (cond ((consp child)
           (dolist (it child)
             (cond ((stringp it) (%append (%make-text-node it)))
                   ((dom-element-p it) (%append it)))))
          ((stringp child)(%append (%make-text-node child)))
          ((dom-element-p child)(%append child))
          (t (error "WTF ~a for append?" child)))
    parent))

;;; remove child from parentNode
;;; return removed element or nil, if parentNode is absent
(defun remove-child (elem)
  (let ((parent (jscl::oget elem "parentNode")))
    (cond ((jscl::js-null-p parent)
           (return-from remove-child nil))
          (t ((jscl::oget parent "removeChild")  elem )))))

;;; mount what where
;;; unix like style: mount what where
(defun mount (what where)
  (node-append-child what where))

;;; umount what
(defun umount (what)
  (remove-child what))


;;; classes

;;; get/set className string
;;;    (get-class-name el) => "a b c" or ""
;;;    (get-class-name el str) returned js-undefined
(defun get-class-name (elem &optional string)
  (cond ((null string)
         (let ((cls (jscl::oget elem "className")))
           (if (zerop (length cls)) nil cls)))
        (t
         (check-type string string)
         (set-Attribute elem "class" string))))

;;; get (list* class-name*)
(defun get-class-list (elem)
  (when (dom-element-p elem)
    (jscl::%lmapcar 'jscl::js-to-lisp
                    (jscl::vector-to-list (jscl::oget elem "classList")))))

;;; true if element contains class-name
(defun contains-class-p (elem class-name)
  (let ((collection (jscl::oget elem "classList")))
    (funcall ((jscl::oget collection "contains" "bind") collection class-name))))

(defun add-class-name (elem class)
  (let ((collection (jscl::oget elem "classList")))
    (funcall ((jscl::oget collection "add" "bind") collection class))
    (values-list nil)))

;;; (remove-class div "visible")
;;; Return: none
(defun remove-class (elem class)
  (let ((collection (jscl::oget elem "classList")))
    (funcall ((jscl::oget collection "remove" "bind") collection class))
    (values-list nil)))

;;; (toggle-class div "visible")
;;; =>
;;;   if class exists - remove it
;;;   else add new class to dom element
;;; Return: none
(defun toggle-class-name (elem class)
  (let ((collection (jscl::oget elem "classList")))
    (funcall ((jscl::oget collection "toggle" "bind") collection class))
    (values-list nil)))

;;; node manipulation

;;; parent node
(defun parent-node (e)
  (when (dom-element-p e)
    (jscl::oget e "parentNode")))

;;; node name
(defun node-name (e)
  (when (dom-element-p e)
    (jscl::oget e "nodeName")))

;;; true if element has childs
(defun has-childs (element)
  (funcall ((jscl::oget element "hasChildNodes" "bind") element)))

;;; => 0 or collection size
(defun childs-count (element)
  (jscl::oget element "childElementCount"))

;;; Get dom element childs collection
;;; or nil
(defun get-childs (element)
  (jscl::vector-to-list (jscl::oget element "childNodes")))


;;; dom:first-element-child
;;;  => [object HTMLDivElement]
;;;     nil
(defun first-child (element)
  (let ((f (jscl::oget element "firstElementChild")))
    (if (jscl::js-null-p f)
        nil
        f)))

;;; dom:last-elemeent-child
;;;  => [object HTMLDivElement]
;;;     nil
(defun last-child (element)
  (let ((f (jscl::oget element "lastElementChild")))
    (if (jscl::js-null-p f)
        nil
        f)))


;;; dom:next-sibling
;;; Return: nil if none sibling or next sibling
(defun next-sibling (element)
  (let ((f (jscl::oget element "nextSibling")))
    (if (jscl::js-null-p f)
        nil
        f)))

;;; inserting

;;; dom:insert-before
(defun insert-before (element new-element)
  (funcall ((jscl::oget element "parentNode" "insertBefore" "bind")
            (jscl::oget element "parentNode")
            new-element
            element)))

;;; dom:insert-after
;;; inserts a new element after the specified element
;;; Note: ERROR condition hasnt right sibling
;;; Workaround:
;;;     If you dont check sibling (dom-element-next-sibling) => nil or [dom element]
;;;     its you problem, catch error (for example: (handler-case (...) (error (msg))) )
(defun insert-after (element new-element)
  (funcall ((jscl::oget element "parentNode" "insertBefore" "bind")
            (jscl::oget element "parentNode")
            new-element
            (jscl::oget element "nextSibling") )))


;;; dom:insert-top
;;; insert new element to top dom stucture
;;; Note: ERROR condition if element hasnt child
(defun insert-top (element new-element)
  (funcall ((jscl::oget element "insertBefore" "bind")
            element
            new-element
            (jscl::oget element "firstElementChild"))))


;;;  Measurement's

;;; client-width in pixels
;;;  eq 0 with no css, hidden
;;;  includes padding
;;;  excludes borders, margins, and vertical scrollbars
(defun element-width (el)
  (jscl::oget el "clientWidth"))

;;; client-height in pixels
;;;  t's the inner height
;;;  includes padding
;;;  excludes borders, margins, and horizontal scrollbars
(defun element-height (el)
  (jscl::oget el "clientHeight"))

;;;  
(defun page-width ()
  (apply 'max
         (list  #j:document:body:scrollWidth
                #j:document:body:offsetWidth
                #j:document:documentElement:scrollWidth
                #j:document:documentElement:offsetWidth
                #j:document:documentElement:clientWidth)))

(defun page-height ()
  (apply 'max
         (list #j:document:body:scrollHeight
                #j:document:body:offsetHeight
                #j:document:documentElement:scrollHeight
                #j:document:documentElement:offsetHeight
                #j:document:documentElement:clientHeight)))

;;; visible < page
(defun visible-width ()
  #j:document:body:clientWidth)

(defun visible-height ()
  #j:document:body:clientHeight)

;;; html producer
;;; set element attribute by js-style path ie "abc.d.e.f"
(defmacro !transform-symbol-name (what)
  `(if (symbolp ,what)
       (setq ,what (symbol-name ,what))))

(defmacro |string| (m s  &body a)
  `(let ((o (jscl::lisp-to-js ,s)))
     ((jscl::oget o ,m "bind") o ,@a)))

(defun |string-split| (src &optional (delim #\space) (max 100)) 
  (jscl::%lmapcar 'jscl::js-to-lisp
                  (jscl::vector-to-list (funcall (|string| "split" src delim max)))))

;;; Element attributes handler's

;;; attribute as JS path.names*
(defun %set-attributes (element path value)
  (!transform-symbol-name path)
  (let ((pl (|string-split| path "."))
        (r))
    (labels ((%w (e p)
               (if (cdr p)
                   (%w (jscl::oget e (car p)) (cdr p))
                   e)))
      (setq r (%w element pl))
      (jscl::oset value r (car (last pl))))))

;;; style attribute as string, may be greater than once
(defun %set-style* (*el value)
  (set-attribute *el "style" value))

;;; internal producer
(defun %html-producer (tag &rest any-others)
  (check-type tag (or symbol string))
  (!transform-symbol-name tag)
  (let* ((*el (#j:window:document:createElement tag)))
    (when (null any-others) (return-from %html-producer *el))
    (flet ((%do (key value)
             (check-type key (or keyword symbol string))
             (case key
               ;; set element style
               ;;   (html:div :style "backrgound-color:red")
               ;; => <div style="backrgound-color:red"></div>
               ;; but 
               ;;   (html:div :|style.background-color| "red")
               ;; => <div style="background-color: red;"></div>
               ((:style) (%set-style* *el value))
               ;; append new element to produced element
               ;; (html:div :append (html::empty-div) :append (html::empty-span) :|id| "xx")
               ;; => <div id="xx"><div></div><span></span></div>
               ((:append) (node-append-child value *el))
               ;; append produced element to other element
               ;; (setq sp (html:span :|id| "span"))
               ;; (html:div :append-to sp)
               ;;  => <span id="span"><div></div></span>
               ((:append-to :mount) (node-append-child *el value))
               ;; make any attribute's
               ;; =>
               ;; (html:div :|id| "xx"
               ;;           :|style.background-color| "red"
               ;;:          :|position| "absolute")
               (otherwise (%set-attributes *el key value)))))
      (let ((elid 0)
            (key-val))
        (dolist (key any-others)
          (if (oddp elid) (%do key-val key)
              (setq key-val key))
          (incf elid)))
      *el)))

;;; create empty html element with tag
(defun create-element (tag) (#j:window:document:createElement tag))

;;; declare html element constructor with tag
;;; after this may to do create and customize elements
;;; and perform operations on them  
;;;
;;; (html:declare-element div)
;;; (html:declare-element span)
;;; (setq div
;;;    (html:div :|id| "first"
;;;              :append (html:span :|id| "second")
;;;              :mount (html:get-element-by-id "top")))
;;;
(defmacro declare-element (name)
  (let* ((element  (intern (symbol-name `,name) "HTML"))
         (tag (symbol-name `,name))
         (pkg (package-name *package*)))
    `(block nil
       (in-package :html)
       (export (list ',element))
       (prog1
        (defun ,element (&rest parms)
          (apply '%html-producer ,tag parms))
        (in-package ,pkg))
       )))

(defun empty-span ()
  (#j:window:document:createElement "span"))

(defun empty-div ()
  (#j:window:document:createElement "div"))

(defun empty-pre ()
  (#j:window:document:createElement "pre"))


(in-package :cl-user)

;;; initial set of the html element contructors
;;; other constructors  are created in the application as needed

(html:declare-element button)
(html:declare-element div)
(html:declare-element span)
(html:declare-element img)
(html:declare-element textarea)

;;; EOF
