;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.ffi.raw1394)

;;; Load the library

(progn
  (cffi:load-foreign-library "/usr/lib/libraw1394.so"))

(cffi:defcfun ("raw1394_get_errcode" raw1394_get_errcode) :int
  (handle raw1394handle_t))

(cffi:defcfun ("raw1394_errcode_to_errno" raw1394_errcode_to_errno) :int
  (errcode :int))

(cffi:defcfun ("raw1394_new_handle" raw1394_new_handle) raw1394handle_t)

(cffi:defcfun ("raw1394_destroy_handle" raw1394_destroy_handle) :void
  (handle raw1394handle_t))

(cffi:defcfun ("raw1394_new_handle_on_port" raw1394_new_handle_on_port) raw1394handle_t
  (port :int))

(cffi:defcfun ("raw1394_get_local_id" raw1394_get_local_id) nodeid_t
  (handle raw1394handle_t))

(cffi:defcfun ("raw1394_get_irm_id" raw1394_get_irm_id) nodeid_t
  (handle raw1394handle_t))

(cffi:defcfun ("raw1394_get_nodecount" raw1394_get_nodecount) :int
  (handle raw1394handle_t))

(cffi:defcfun ("raw1394_reset_bus" raw1394_reset_bus) :int
  (handle raw1394handle_t))

(cffi:defcfun ("raw1394_loop_iterate" raw1394_loop_iterate) :int
  (handle raw1394handle_t))

(cffi:defcfun ("raw1394_read" raw1394_read) :int
  (handle raw1394handle_t)
  (node nodeid_t)
  (addr nodeaddr_t)
  (length size_t)
  (buffer :pointer))

(cffi:defcfun ("raw1394_write" raw1394_write) :int
  (handle raw1394handle_t)
  (node nodeid_t)
  (addr nodeaddr_t)
  (length size_t)
  (data :pointer))

;;; EOF
