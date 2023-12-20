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
;;;              (load "pathnames" :hook bin :output "wam.js")
;;; 
(in-package :ffi)


;;; WebAssemble
(export '(ffi::web-assemble-memory))
(defun web-assemble-memory (&key (initial 1) (maximum 2) (shared nil))
    (jscl::make-new #j:WebAssembly:Memory
                    (make-obj "initial" initial "maximum" maximum "shared" shared)))

;;; return ArrayBuffer object
(export '(ffi::wa-mem-buffer))
(defun wa-mem-buffer (mem)
  (jscl::oget mem "buffer"))

;;; Increases the size of the memory instance by a specified number of WebAssembly pages 
(export '(ffi::wa-mem-grow))
(defun wa-mem-grow (mem &optional (pages 1))
  ((jscl::oget mem "grow") pages))

#|
(setq mem (ffi:web-assemble-memory :initial 1 :maximum 1 :shared t))
(setq a32 (jscl::make-new #j:Int32Array (ffi:wam-buffer mem)))
(#j:Atomics:add a32 0 1)
|#

;;; WebAssembly:instantiateStreaming
;;; https://developer.mozilla.org/en-US/docs/WebAssembly/JavaScript_interface/instantiateStreaming
;;; return
;;; A Promise that resolves to a ResultObject which contains two fields:
;;;   module: A WebAssembly.Module object representing the compiled WebAssembly module.
;;;           This Module can be instantiated again or shared via postMessage().
;;;   instance: A WebAssembly.Instance object that contains all the Exported WebAssembly functions.

(export '(ffi::wa-fetch))
(defun wa-fetch (pathname &optional (import nil))
  (if import
   (#j:WebAssembly:instantiateStreaming (#j:fetch pathname) import)
   (#j:WebAssembly:instantiateStreaming (#j:fetch pathname))))

#|

Example

look at: https://github.com/mdn/webassembly-examples/blob/main/js-api-examples/instantiate-streaming.html

(ffi:promise-then 
 (ffi:wa-fetch "./simple.wasm"
           (ffi::%def-obj-template "{imports: { imported_func: arg => { console.log('aaa',arg);}}};"))
 (lambda (res)
   ((jscl::oget res "instance" "exports" "exported_func")))
 (lambda (res)
   (print (list :something-went-wrong res))
   (terpri))
 )

|#

;;; EOF
