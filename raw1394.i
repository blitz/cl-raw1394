/* SWIG interface to libraw1394

   Adapted from ../pythonraw1394/raw1394.i
*/

%module raw1394
%{
#include <ieee1394.h>
#include <raw1394.h>
#include <csr.h>
%}

%ignore raw1394_set_iso_handler;
%ignore raw1394_start_iso_rcv;
%ignore raw1394_stop_iso_rcv;
%ignore raw1394_start_iso_write;
%ignore raw1394_iso_write;

%include "ieee1394.h"
#define __attribute__(x)
%include "raw1394.h"
%include "csr.h"


 /* EOF */
