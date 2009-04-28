;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.ffi.raw1394)

(defclass* raw1394-port ()
  (handle))

(defclass* raw1394-node ()
  (port
   number))

(defun make-port-finalizer (handle)
  (lambda ()
    (warn "Destroying raw1394 in finalizer.")
    (raw1394_destroy_handle handle)))

(defmethod initialize-instance :after ((o raw1394-port) &key (port 0) &allow-other-keys)
  (unless (slot-boundp o 'handle)
    (let ((handle (raw1394_new_handle_on_port port)))
      (when (zerop handle)
        (error 'raw1394-error 
               :reason "Could not get raw1394 handle."))
      (setf (handle-of o) handle)
      #+ sbcl (sb-ext:finalize o (make-port-finalizer (handle-of o))
                               :dont-save t))))

(defmethod raw1394-bus-id ((o raw1394-port))
  (ash (logand #xFFC0 (raw1394_get_local_id (handle-of o)))
       -6))

(defmethod raw1394-node-count ((o raw1394-port))
  (raw1394_get_nodecount (handle-of o)))

(defmethod raw1394-destroy ((o raw1394-port))
  #+ sbcl (sb-ext:cancel-finalization o)
  (raw1394_destroy_handle (handle-of o)))


(defun raw1394-node (port number)
  (make-instance 'raw1394-node
                 :port port
                 :number number))

(defmethod raw1394-max-request ((node raw1394-node))
  ;; XXX Should check config rom
  512)

(defmethod raw1394-node-id ((node raw1394-node))
  (logior (ash (raw1394-bus-id (port-of node)) 6)
          (number-of node)))

(defmethod raw1394-read ((node raw1394-node) address size)
  (assert (<= size (raw1394-max-request node)))
  (with-foreign-object (buf :unsigned-char size)
    (let* ((h (handle-of (port-of node)))
           (res (raw1394_read h
                             (raw1394-node-id node)
                             address
                             size
                             buf)))
      (when  (= -1 res)
        (let* ((errcode (raw1394_get_errcode h))
               (errno (raw1394_errcode_to_errno errcode)))
          (error 'raw1394-read-error 
                 :reason "Error in raw1394_read: Error code ~A, errno ~A"
                 :address address
                 :errcode errcode 
                 :errno errno)))
      (loop 
         with result = (make-array size :element-type '(unsigned-byte 8))
         for i from 0 below size
         do (setf (aref result i) (mem-aref buf :unsigned-char i))
         finally (return result)))))

(defun raw1394-write (node address buffer)
  (with-foreign-object (buf :unsigned-char (length buffer))
    (loop 
       for i from 0 below (length buffer)
       do (setf (mem-aref buf :unsigned-char i) (aref buffer i)))
    (let ((res (raw1394_write (handle-of (port-of node))
                              (raw1394-node-id node)
                              address
                              (length buffer)
                              buf)))
      (unless (zerop res)
        (let* ((errcode (raw1394_get_errcode (handle-of (port-of node))))
               (errno (raw1394_errcode_to_errno errcode)))
          (error 'raw1394-write-error 
                 :reason "raw1394_write"
                 :address address
                 :errcode errcode 
                 :errno errno)))
      (values))))

;;; Finding specific nodes

(defclass* raw1394-guid ()
  ((guid-vector :type (simple-array (unsigned-byte 8) (8)))))


(defmethod print-object ((object raw1394-guid) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~{~2,'0X~}" (coerce (guid-vector-of object) 'list))))

(defun raw1394-guid-from-string (str)
  (unless (= (length str) (* 2 8))
    (error "Invalid GUID string"))
  (loop 
     with out = (make-array 8 :element-type '(unsigned-byte 8))
     for index upfrom 0 below (truncate (length str) 2)
     for str-index = (* 2 index)
     do (setf (aref out index) (parse-integer str
                                              :radix 16
                                              :start str-index
                                              :end (+ 2 str-index)))
       
     finally (return (make-instance 'raw1394-guid :guid-vector out))))

(defun raw1394-guid-equal (guid1 guid2)
  (loop 
     with v1 = (guid-vector-of guid1)
     with v2 = (guid-vector-of guid2)
     for i from 0 below 8
     always (= (aref v1 i)
               (aref v2 i))))


(defmethod raw1394-node-guid ((node raw1394-node))
  (let ((out (make-array 8 :element-type '(unsigned-byte 8))))
    ;; Only quadlet sized accesses are guaranteed to work in the
    ;; config rom.
    (setf (subseq out 0) (raw1394-read node (+ +csr-register-base+
                                               +csr-config-rom+
                                               (* 3 4))
                                       4)
          (subseq out 4) (raw1394-read node (+ +csr-register-base+
                                               +csr-config-rom+
                                               (* 4 4))
                                       4))
    (make-instance 'raw1394-guid :guid-vector out)))

(defmethod raw1394-map-nodes ((port raw1394-port) fn)
  (loop 
     for n from 0 below (raw1394-node-count port)
     do (let ((node (make-instance 'raw1394-node 
                                   :port port
                                   :number n)))
          (funcall fn port node))))

(defmethod raw1394-find-node ((port raw1394-port) predicate-fn)
  (raw1394-map-nodes port (lambda (port node)
                            (when (funcall predicate-fn port node)
                              (return-from raw1394-find-node
                                node)))))


;;; EOF
