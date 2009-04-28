;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(defpackage :blitz.ffi.raw1394
  (:use :common-lisp :cffi :defclass-star)
  (:export #:raw1394-port
           #:raw1394-node
           #:raw1394-destroy
           #:raw1394-node
           #:raw1394-max-request
           #:raw1394-read
           #:raw1394-write
           #:raw1394-map-nodes
           #:raw1394-node-id
           #:raw1394-node-guid
           #:raw1394-find-node
           #:raw1394-guid-from-string
           #:raw1394-guid-equal
           ;; Errors
           #:raw1394-error
           #:raw1394-io-error
           #:raw1394-read-error
           #:raw1394-write-error
           ))
(in-package :blitz.ffi.raw1394)

;;; EOF
