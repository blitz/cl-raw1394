;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(defpackage :blitz.ffi.raw1394.system
  (:use :common-lisp :asdf))
(in-package :blitz.ffi.raw1394.system)

 ;;; CFFI-Grovel is needed for processing grovel-file components
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(defsystem :cl-raw1394
  :depends-on (:cffi :defclass-star)
  :description "This is currently a very minimal binding for libraw1394."
  :author "Julian Stecklina"
  :components ((:file "packages")
               (:file "errors" :depends-on ("packages"))
               (cffi-grovel:grovel-file "raw1394-grovel" :depends-on ("packages"))
               (:file "raw1394" :depends-on ("errors" "raw1394-grovel"))
               (:file "convenience" :depends-on ("raw1394"))))

;;; EOF
