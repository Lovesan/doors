;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010-2011, Dmitry Ignatiev <lovesan.ru@gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE

(in-package #:doors)

(define-struct (page-file-information
                 (:constructor make-page-file-info
                               (&key total-size total-in-use peak-usage))
                 (:conc-name page-file-info-))
    "Contains information about a pagefile."
  (cb dword :initform (sizeof 'page-file-information))
  (reserved dword)
  (total-size size-t)
  (total-in-use size-t)
  (peak-usage size-t))

(define-struct (module-info
                 (:conc-name module-))
    "Contains the module load address, size, and entry point."
  (base-of-dll pointer)
  (size-of-image dword)
  (entry-point pointer))

(define-struct (performance-information
                 (:conc-name perf-info-))
    "Contains performance information."
  (cb dword :initform (sizeof 'performance-information))
  (commit-total size-t)
  (commit-limit size-t)
  (commit-peak size-t)
  (physical-total size-t)
  (physical-available size-t)
  (system-cache size-t)
  (kernel-total size-t)
  (kernel-paged size-t)
  (kernel-nonpaged size-t)
  (page-size size-t)
  (handle-count dword)
  (process-count dword)
  (thread-count dword))

(define-struct (process-memory-counters
                 (:conc-name process-mc-)
                 (:constructor make-process-memory-counters
                               (&key page-fault-count peak-working-set-size
                                working-set-size quota-paged-pool-usage
                                quota-peak-paged-pool-usage
                                quota-nonpaged-pool-usage
                                pagefile-usage
                                peak-pagefile-usage)))
    "Contains the memory statistics for a process."
  (cb dword :initform (sizeof 'process-memory-counters))
  (page-fault-count dword)
  (peak-working-set-size size-t)
  (working-set-size size-t)
  (quota-peak-paged-pool-usage size-t)
  (quota-paged-pool-usage size-t)
  (quota-peak-nonpaged-pool-usage size-t)
  (quota-nonpaged-pool-usage size-t)
  (pagefile-usage size-t)
  (peak-pagefile-usage size-t))

(define-struct (process-memory-counters*
                 (:conc-name process-mc-)
                 (:include process-memory-counters)
                 (:constructor make-process-memory-counters*
                               (&key page-fault-count peak-working-set-size
                                working-set-size quota-paged-pool-usage
                                quota-peak-paged-pool-usage
                                quota-nonpaged-pool-usage
                                pagefile-usage
                                peak-pagefile-usage
                                private-usage
                                &aux (cb (sizeof 'process-memory-counters*)))))
    "Contains extended memory statistics for a process."
  (private-usage size-t))

(define-enum (ws-block-protection-flags
               (:list t)
               (:base-type ulong-ptr)
               (:conc-name ws-block-))
  (:read          #b00001)
  (:execute       #b00010)
  (:read/write    #b00100)
  (:copy-on-write #b00101)
  (:non-cacheable #b01000)
  (:guard-page    #b10000))

(define-union (working-set-block-information
                 (:conc-name ws-block-info-)
                 (:reader %ws-block-info-reader)
                 (:writer %ws-block-info-writer))
    "Contains working set information for a page."  
  (virtual-page ulong-ptr)
  (protection ws-block-protection-flags)
  (share-count byte)
  (shared-p (boolean ulong-ptr)))

(defun %ws-block-info-reader (pointer out)
  (declare (type pointer pointer))
  (let* ((out (or out (make-working-set-block-information)))
         (flags (deref pointer 'ulong-ptr)))
    (declare (type working-set-block-information out)
             (type ulong-ptr flags))
    (setf (ws-block-info-protection out)
          (translate (ldb (byte 5 0) flags)
                     'ws-block-protection-flags)
          (ws-block-info-share-count out)
          (ldb (byte 3 5) flags)
          (ws-block-info-shared-p out)
          (logbitp 8 flags)
          (ws-block-info-virtual-page out)
          (ash flags -12))
    out))

(defun %ws-block-info-writer (value pointer)
  (declare (type pointer pointer)
           (type working-set-block-information value))
  (let ((flags 0))
    (declare (type ulong-ptr flags))
    (setf (ldb (byte 5 0) flags)
          (convert (ws-block-info-protection value)
                   'ws-block-protection-flags)
          (ldb (byte 3 5) flags)
          (ws-block-info-share-count value)
          (ldb (byte 1 8) flags)
          (if (ws-block-info-shared-p value) 1 0)
          flags (logior flags (ash (ws-block-info-virtual-page value) 12))
          (deref pointer 'ulong-ptr) flags))
  value)

(define-union (working-set-block-information*
                 (:conc-name ws-block-info-)
                 (:reader %ws-block-info-reader*)
                 (:writer %ws-block-info-writer*))
    "Contains extended working set information for a page."
  (node ulong-ptr)
  (valid-p (boolean ulong-ptr))
  (share-count* ulong-ptr)
  (protection* memory-protection-flags)
  (shared-p* (boolean ulong-ptr))
  (locked-p (boolean ulong-ptr))
  (large-page-p (boolean ulong-ptr)))

(defun %ws-block-info-reader* (pointer out)
  (declare (type pointer pointer))
  (let ((out (or out (make-working-set-block-information*)))
        (flags (deref pointer 'ulong-ptr)))
    (declare (type working-set-block-information* out)
             (type ulong-ptr flags))
    (setf (ws-block-info-valid-p out)
          (logbitp 0 flags)
          (ws-block-info-share-count* out)
          (ldb (byte 3 1) flags)
          (ws-block-info-shared-p* out)
          (logbitp 15 flags)
          (ws-block-info-protection* out)
          (translate (ldb (byte 11 4) flags)
                     'memory-protection-flags)
          (ws-block-info-node out)
          (ldb (byte 6 16) flags)
          (ws-block-info-locked-p out)
          (logbitp 22 flags)
          (ws-block-info-large-page-p out)
          (logbitp 23 flags))
    out))

(defun %ws-block-info-writer* (value pointer)
  (declare (type pointer pointer)
           (type working-set-block-information* value))
  (let ((flags 0))
    (declare (type ulong-ptr flags))
    (setf (ldb (byte 1 0) flags)
          (if (ws-block-info-valid-p value) 1 0)
          (ldb (byte 3 1) flags)
          (ws-block-info-share-count* value)
          (ldb (byte 11 4) flags)
          (convert (ws-block-info-protection* value)
                   'memory-protection-flags)
          (ldb (byte 1 15) flags)
          (if (ws-block-info-shared-p* value) 1 0)
          (ldb (byte 6 16) flags)
          (ws-block-info-node value)
          (ldb (byte 1 22) flags)
          (if (ws-block-info-locked-p value) 1 0)
          (ldb (byte 1 23) flags)
          (if (ws-block-info-large-page-p value) 1 0)
          (deref pointer 'ulong-ptr) flags))
  value)

(define-struct (working-set-information*
                 (:conc-name ws-info-))
    "Contains extended working set information for a process."
  (virtual-address pointer)
  (virtual-attributes working-set-block-information*))

(define-struct (ws-watch-information
                 (:conc-name ws-watch-info-))
    "Contains information about a page added to a process working set."
  (faulting-pc pointer)
  (faulting-va pointer))

(define-struct (ws-watch-information*
                 (:include ws-watch-information)
                 (:constructor make-ws-watch-information*
                               (&key faulting-pc
                                     faulting-va
                                     faulting-thread-id))
                 (:conc-name ws-watch-info-))
    "Contains extended information about a page added to a process working set."
  (faulting-thread-id ulong-ptr)
  (reserved ulong-ptr))

(define-external-function
    ("EmptyWorkingSet" (:camel-case))
    (:stdcall psapi)
  ((last-error bool))
  "Removes as many pages as possible from the working set of the specified process."
  (process handle :optional current-process))

(define-external-function
    ("EnumDeviceDrivers" (:camel-case))
    (:stdcall psapi)
  ((last-error bool) rv
   (external-function-call "EnumDeviceDrivers"
     ((:stdcall psapi)
      ((last-error bool) rv (if (/= needed %needed)
                              (subseq buffer 0 (floor needed (sizeof '*)))
                              buffer))
      ((& (simple-array pointer) :out) buffer
       :aux (make-array (floor %needed (sizeof '*))
              :element-type 'pointer
              :initial-element &0))
      (dword cb :aux %needed)
      ((& dword :out) needed :aux))))
  "Retrieves the load address for each device driver in the system."
  (%image-base pointer :aux)
  (%cb dword :aux)
  (%needed (& dword :inout) :aux))

(define-external-function
    (#+doors.unicode "GetDeviceDriverFileNameW"
     #-doors.unicode "GetDeviceDriverFileNameA"
      device-driver-file-name)
    (:stdcall psapi)
  ((last-error dword not-zero) rv
   (subseq filename 0 rv))
  "Retrieves the path available for the specified device driver."
  (image-base pointer)
  (filename (& tstring :out) :aux (make-string buffer-length))
  (buffer-length dword :optional 256))

#-win2000
(define-external-function
    (#+doors.unicode "EnumPageFilesW"
     #-doors.unicode "EnumPageFilesA"
                   enum-page-files)
    (:stdcall psapi)
  ((last-error bool))
"Calls the callback routine for each installed pagefile in the system.
Callback signature: (:stdcall boolean
                        ((context pointer)
                         (page-file-info (& page-file-information))
                         (filename (& tstring))))"
  (callback pointer)
  (context  pointer))

(define-external-function
    ("EnumProcesses" (:camel-case))
    (:stdcall psapi)
  ((last-error bool) rv (floor bytes-returned (sizeof 'dword)))
  "Retrieves the process identifier for each process object in the system."
  (buffer (& (array dword) :out))
  (buffer-size dword :optional (array-total-size buffer))
  (bytes-returned (& dword :inout)
                  :aux (setf buffer-size
                             (* buffer-size (sizeof 'dword)))))

(define-external-function
    ("EnumProcessModules" (:camel-case))
    (:stdcall psapi)
  ((last-error bool) rv
   (external-function-call "EnumProcessModules"
     ((:stdcall psapi)
      ((last-error bool) rv
       (if (/= needed %needed)
         (subseq modules 0 (floor needed (sizeof 'pointer)))
         modules))
      (handle         %process :aux process)
      ((& (simple-array handle) :out)
       modules :aux (make-array (floor %needed (sizeof 'pointer))
                      :element-type 'pointer :initial-element &0))
      (dword          cb       :aux %needed)
      ((& dword :out) needed   :aux))))
  "Retrieves a handle for each module in the specified process."
  (process handle :optional current-process)
  (%modules pointer :aux &0)
  (%cb dword :aux 0)
  (%needed (& dword :out) :aux))

(define-enum (list-modules-flag
               (:conc-name list-modules-)
               (:base-type dword))
  (:default 0)
  (:32bit   1)
  (:64bit   2)
  (:all     3))

#-(or win2000 winxp winserver2003 winxp64 winhomeserver)
(define-external-function
    ("EnumProcessModulesEx" enum-process-modules*)
    (:stdcall psapi)
  ((last-error bool) rv
     (external-function-call "EnumProcessModulesEx"
       ((:stdcall psapi)
        ((last-error bool) rv (if (< needed %needed)
                                (subseq buffer (floor needed (sizeof 'pointer)))
                                buffer))
        (handle %process :aux process)
        ((& (simple-array pointer) :out) buffer
         :aux (make-array (floor %needed (sizeof 'pointer))
                :element-type 'pointer :initial-element &0))
        (dword cb :aux %needed)
        ((& dword :out) needed :aux)
        (list-modules-flag %filter-flag :aux filter-flag))))
  "Retrieves a handle for each module in the specified process that meets the specified filter criteria."
  (process handle :optional current-process)
  (%modules pointer :aux &0)
  (%cb dword :aux 0)
  (%needed (& dword :out) :aux)
  (filter-flag list-modules-flag))

(define-external-function
    (#+doors.unicode "GetDeviceDriverBaseNameW"
     #-doors.unicode "GetDeviceDriverBaseNameA"
                   device-driver-base-name)
    (:stdcall psapi)
  ((last-error dword not-zero) rv (subseq buffer 0 rv))
  "Retrieves the base name of the specified device driver."
  (image-base pointer)
  (buffer (& tstring :out) :aux (make-string buffer-size))
  (buffer-size dword :optional 256))

(define-external-function
    (#+doors.unicode "GetMappedFileNameW"
     #-doors.unicode "GetMappedFileNameA"
                   mapped-file-name)
    (:stdcall psapi)
  ((last-error dword not-zero) rv (subseq buffer 0 rv))
  "Returns the name of the memory-mapped file."
  (process handle :optional current-process)
  (address pointer)
  (buffer (& tstring :out) :aux (make-string buffer-size))
  (buffer-size dword :optional 256))

(define-external-function
    (#+doors.unicode "GetModuleBaseNameW"
     #-doors.unicode "GetModuleBaseNameA"
                   module-base-name)
    (:stdcall psapi)
  ((last-error dword not-zero) rv (subseq buffer 0 rv))
  "Retrieves the base name of the specified module."
  (process handle :optional current-process)
  (module handle :optional)
  (buffer (& tstring :out) :aux (make-string buffer-size))
  (buffer-size dword :optional 256))

(define-symbol-macro module-base-name (module-base-name))

(define-external-function
    (#+doors.unicode "GetModuleFileNameExW"
     #-doors.unicode "GetModuleFileNameExA"
                   module-file-name*)
    (:stdcall psapi)
  ((last-error dword not-zero) rv (subseq buffer 0 rv))
  "Retrieves the fully-qualified path for the file containing the specified module."
  (process handle :optional current-process)
  (module handle :optional)
  (buffer (& tstring :out) :aux (make-string buffer-size))
  (buffer-size dword :optional 256))

(define-symbol-macro module-file-name* (module-file-name*))

(define-external-function
    ("GetModuleInformation" module-information)
    (:stdcall psapi)
  ((last-error bool) rv info)
  "Retrieves information about the specified module."
  (process handle :optional current-process)
  (module handle :optional)
  (info (& module-info :out) :aux)
  (cb dword :aux (sizeof 'module-info)))

(define-symbol-macro module-information (module-information))

#-win2000
(define-external-function
    ("GetPerformanceInfo" performance-info)
    (:stdcall psapi)
  ((last-error bool) rv info)
  "Retrieves the performance information."
  (info (& performance-information :out) :aux)
  (cb dword :aux (sizeof 'performance-information)))

#-win2000
(define-symbol-macro performance-info (performance-info))

#-win2000
(define-external-function
    (#+doors.unicode "GetProcessImageFileNameW"
     #-doors.unicode "GetProcessImageFileNameA"
                   process-image-file-name)
    (:stdcall psapi)
  ((last-error dword not-zero) rv (subseq buffer 0 rv))
  "Retrieves the name of the executable file for the specified process."
  (process handle :optional current-process)
  (buffer (& tstring :out) :aux (make-string buffer-size))
  (buffer-size dword :optional 256))

#-win2000
(define-symbol-macro process-image-file-name (process-image-file-name))

(define-external-function
    ("GetProcessMemoryInfo" process-memory-info)
    (:stdcall psapi)
  ((last-error bool) rv info)
  "Retrieves information about the memory usage of the specified process."
  (process handle :optional current-process)
  (info (& (union ()
             (mc* process-memory-counters*)
             (mc process-memory-counters))
         :out)
        :optional (make-process-memory-counters))
  (cb dword :aux (process-mc-cb info)))

(define-symbol-macro process-memory-info (process-memory-info))

(define-external-function
    ("GetWsChanges" ws-changes)
    (:stdcall psapi)
  ((last-error bool))
  "Retrieves information about the pages that have been added to the working set of the specified process since the last time this function or the initialize-process-for-ws-watch function was called."
  (process handle :optional current-process)
  (watch-info (& (array ws-watch-information) :out))
  (cb dword :optional (* (sizeof 'ws-watch-information)
                         (array-total-size watch-info))))

#-(or win2000 winxp winserver2003 winxp64 winhomeserver)
(define-external-function
    ("GetWsChangesEx" ws-changes*)
    (:stdcall psapi)
  ((last-error bool))
  "Retrieves information about the pages that have been added to the working set of the specified process since the last time this function or the initialize-process-for-ws-watch function was called."
  (process handle :optional current-process)
  (watch-info (& (array ws-watch-information*) :out))
  (cb dword :optional (* (sizeof 'ws-watch-information*)
                         (array-total-size watch-info))))

(define-external-function
    ("InitializeProcessForWsWatch" (:camel-case))
    (:stdcall psapi)
  ((last-error bool))
  "Initiates monitoring of the working set of the specified process. "
  (process handle :optional current-process))

(define-external-function
    ("QueryWorkingSet" (:camel-case))
    (:stdcall psapi)
  ((last-error bool) rv (and (> cb (sizeof 'ulong-ptr))
                             (<= (row-major-aref buffer 0)
                                 (1- (floor cb (sizeof 'ulong-ptr))))))
  "Retrieves information about the pages currently added to the working set of the specified process."
  (process handle :optional current-process)
  (buffer (& (array ulong-ptr) :out))
  (cb dword :optional (* (sizeof 'ulong-ptr)
                         (array-total-size buffer))))

#-(or win2000 winxp winhomeserver)
(define-external-function
    ("QueryWorkingSetEx" query-working-set*)
    (:stdcall psapi)
  ((last-error bool))
  "Retrieves extended information about the pages at specific virtual addresses in the address space of the specified process."
  (process handle :optional current-process)
  (buffer (& (array working-set-information*) :inout))
  (cb dword :optional (* (sizeof 'working-set-information*)
                         (array-total-size buffer))))
