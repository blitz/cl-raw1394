;;; -*- Mode: Lisp -*-
;;; CFFI grovel file

(in-package :blitz.ffi.raw1394)

(include "libraw1394/csr.h")
(include "libraw1394/ieee1394.h")
(include "libraw1394/raw1394.h")

;;; Constants

(constant (+csr-register-base+ "CSR_REGISTER_BASE"))
(constant (+csr-config-rom+ "CSR_CONFIG_ROM"))
(constant (+csr-config-rom-end+ "CSR_CONFIG_ROM_END"))

;;; Types

(ctype size_t "size_t")
(ctype byte_t "byte_t")
(ctype quadlet_t "quadlet_t")
(ctype octlet_t "octlet_t")
(ctype nodeaddr_t "nodeaddr_t")
(ctype nodeid_t "nodeid_t")
(ctype phyid_t "phyid_t")
(ctype raw1394handle_t "raw1394handle_t")
(ctype raw1394_errcode_t "raw1394_errcode_t")

;;; EOF
