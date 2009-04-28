;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.ffi.raw1394)

(defcondition* raw1394-error (error)
  ((errno 0) 
   (errcode 0)
   reason))

(defcondition* raw1394-io-error (raw1394-error)
  (address))

(defcondition* raw1394-read-error (raw1394-io-error)
  ())

(defcondition* raw1394-write-error (raw1394-io-error)
  ())

;;; EOF
