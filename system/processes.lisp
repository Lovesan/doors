;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010, Dmitry Ignatiev <lovesan.ru@gmail.com>

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

(define-enum (processor-cache-type
               (:list t)
               (:conc-name nil))
  :cache-unified
  :cache-instruction
  :cache-data
  :cache-trace)

(define-struct (cache-descriptor
                 (:conc-name cache-))
    "Describes the cache attributes."
  (level byte)
  (associativity (enum (:base-type byte)
                   (:fully-associative #xFF)))
  (line-size word)
  (size dword)
  (type processor-cache-type))

(define-struct (io-counters (:conc-name nil))
    "Contains I/O accounting information for a process or a job object."
  (read-operation-count ullong)
  (write-operation-count ullong)
  (other-operation-count ullong)
  (read-transfer-count ullong)
  (write-transfer-count ullong)
  (other-transfer-count ullong))

(define-struct (process-information
                 (:conc-name process-info-))
    "Contains information about a newly created process and its primary thread."
  (handle handle)
  (thread-handle handle)
  (id dword)
  (thread-id dword))

(define-enum (startup-flags
               (:base-type dword)
               (:list t)
               (:conc-name start-))
  (:force-on-feedback #x00000040)
  (:force-off-feedback #x00000080)
  (:prevent-pinning #x00002000)
  (:run-fullscreen #x00000020)
  (:title-is-app-id #x00001000)
  (:title-is-link-name #x00000800)
  (:use-count-chars #x00000008)
  (:use-file-attribute #x00000010)
  (:use-hotkey #x00000200)
  (:use-position #x00000004)
  (:use-show-window #x00000001)
  (:use-size #x00000002)
  (:use-std-handles #x00000100))

(define-struct (startup-info
                 (:cleaner %startup-info-cleaner)
                 (:writer %startup-info-writer)
                 (:constructor make-startup-info
                               (&key desktop title x y x-size y-size
                                x-count-chars y-count-chars fill-attribute
                                flags show-window stdin stdout stderror)))
    "Specifies the window station, desktop, standard handles, and appearance of the main window for a process at creation time."
  (cb dword :initform (sizeof 'startup-info))
  (reserved pointer :initform &0)
  (desktop (& (const tstring) :in t) :initform void)
  (title (& (const tstring) :in t) :initform void)
  (x dword)
  (y dword)
  (x-size dword)
  (y-size dword)
  (x-count-chars dword)
  (y-count-chars dword)
  (fill-attribute (enum (:list t :base-type dword)
                    (:foreground-blue #x0001)
                    (:foreground-green #x0002)
                    (:foreground-red #x0004)
                    (:foreground-intensity #x0008)
                    (:background-blue #x0010)
                    (:background-green #x0020)
                    (:background-red   #x0040)
                    (:background-intensity #x0080)
                    (:common-lvb-leading-byte #x0100)
                    (:common-lvb-trailing-byte #x0200)
                    (:common-lvb-grid-horizontal #x0400)
                    (:common-lvb-grid-lvertical #x0800)
                    (:common-lvb-grid-rvertical #x1000)
                    (:common-lvb-reverse-video #x4000)
                    (:common-lvb-underscore #x8000)))
  (flags startup-flags)
  (show-window word)
  (reserved2 word)
  (reserved3 pointer)
  (stdin handle)
  (stdout handle)
  (stderror handle))

(declaim (inline %startup-info-cleaner))
(defun %startup-info-cleaner (pointer value)
  (declare (ignore pointer value))
  nil)

;;This is neccesary because extensive inlining of big structure translators
;;cause slow compilation and even may cause some lisp compilers to hang or crash
(defun %startup-info-writer (value pointer)
  (declare (type startup-info value)
           (type pointer pointer))
  (setf (deref pointer 'dword (offsetof 'startup-info 'cb))
        (startup-info-cb value)
        (deref pointer 'size-t (offsetof 'startup-info 'reserved))
        0
        (deref pointer '(& tstring :in t) (offsetof 'startup-info 'desktop))
        (startup-info-desktop value)
        (deref pointer '(& tstring :in t) (offsetof 'startup-info 'title))
        (startup-info-title value)
        (deref pointer 'dword (offsetof 'startup-info 'x))
        (startup-info-x value)
        (deref pointer 'dword (offsetof 'startup-info 'y))
        (startup-info-y value)
        (deref pointer 'dword (offsetof 'startup-info 'x-size))
        (startup-info-x-size value)
        (deref pointer 'dword (offsetof 'startup-info 'y-size))
        (startup-info-y-size value)
        (deref pointer 'dword (offsetof 'startup-info 'x-count-chars))
        (startup-info-x-count-chars value)
        (deref pointer 'dword (offsetof 'startup-info 'y-count-chars))
        (startup-info-y-count-chars value)
        (deref pointer 'dword (offsetof 'startup-info 'fill-attribute))
        (convert (startup-info-fill-attribute value)
                 'char-attributes)
        (deref pointer 'startup-flags (offsetof 'startup-info 'flags))
        (startup-info-flags value)
        (deref pointer 'word (offsetof 'startup-info 'show-window))
        (startup-info-show-window value)
        (deref pointer 'word (offsetof 'startup-info 'reserved2))
        0
        (deref pointer 'size-t (offsetof 'startup-info 'reserved3))
        0
        (deref pointer 'handle (offsetof 'startup-info 'stdin))
        (startup-info-stdin value)
        (deref pointer 'handle (offsetof 'startup-info 'stdout))
        (startup-info-stdout value)
        (deref pointer 'handle (offsetof 'startup-info 'stderror))
        (startup-info-stderror value))
  value)

(define-struct thread-attribute-list
  (pointer pointer))

(define-struct (startup-info*
                 (:include startup-info)
                 (:cleaner %startup-info-cleaner)
                 (:constructor make-startup-info*
                               (&key desktop title x y x-size y-size
                                x-count-chars y-count-chars fill-attribute
                                flags show-window stdin stdout stderror
                                attribute-list &aux (cb (sizeof 'startup-info*)))))
    "Specifies the window station, desktop, standard handles, and attributes for a new process. "
  (attribute-list thread-attribute-list))

(define-enum (process-creation-flags
               (:conc-name create-)
               (:list t)
               (:base-type dword))
  (:break-away-from-job #x01000000)
  (:default-error-mode #x04000000)
  (:new-console #x00000010)
  (:new-process-group #x00000200)
  (:no-window #x08000000)
  (:protected-process #x00040000)
  (:preserve-code-authz-level #x02000000)
  (:shared-wow-vdm #x00001000)
  (:suspended #x00000004)
  (:unicode-environment #x00000400)
  (:debug-only-this-process #x00000002)
  (:debug-process #x00000001)
  (:detached-process #x00000008)
  (:extended-startup-info-present #x00080000)
  (:inherit-parent-affinity #x00010000))

(define-external-function
    (#+doors.unicode "CreateProcessW"
     #-doors.unicode "CreateProcessA"
                   create-process)
    (:stdcall kernel32)
  ((last-error bool) rv process-information)
  "Creates a new process and its primary thread. The new process runs in the security context of the calling process."
  (application-name (& tstring :in t))
  #-doors.unicode
  (command-line (& astring :in t) :key void)
  #+doors.unicode
  (command-line pointer :key)
  (process-attributes (& doors.security:security-attributes :in t)
                      :key void)
  (thread-attributes (& doors.security:security-attributes :in t)
                     :key void)
  (inherit-handles boolean)
  (creation-flags process-creation-flags)
  (environment pointer :key)
  (current-directory (& tstring :in t) :key)
  (startup-info (& (union ()
                          (info* startup-info*)
                          (info startup-info)))
                :key (make-startup-info))
  (process-information (& process-information :out) :aux))

(define-external-function
    (#+doors.unicode "CreateProcessAsUserW"
     #-doors.unicode "CreateProcessAsUserA"
                   create-process-as-user)
    (:stdcall advapi32)
  ((last-error bool) rv process-information)
  "Creates a new process and its primary thread. The new process runs in the security context of the user represented by the specified token."
  (token handle :key)
  (application-name (& tstring :in t))
  #-doors.unicode
  (command-line (& astring :in t) :key void)
  #+doors.unicode
  (command-line pointer :key)
  (process-attributes (& doors.security:security-attributes :in t)
                      :key void)
  (thread-attributes (& doors.security:security-attributes :in t)
                     :key void)
  (inherit-handles boolean)
  (creation-flags process-creation-flags)
  (environment pointer :key)
  (current-directory (& tstring :in t) :key)
  (startup-info (& (union ()
                          (info* startup-info*)
                          (info startup-info)))
                :key (make-startup-info))
  (process-information (& process-information :out) :aux))

(define-external-function
    ("CreateProcessWithLogonW"
      create-process-with-logon)
    (:stdcall advapi32)
  ((last-error bool) rv process-info)
  "Creates a new process and its primary thread. Then the new process runs the specified executable file in the security context of the specified credentials (user, domain, and password). "
  (username (& wstring))
  (domain (& wstring :in t) :key)
  (password (& wstring))
  (logon-flags (enum (:base-type dword)
                 (:with-profile #x00000001)
                 (:net-credentials-only #x00000002)))
  (application-name (& wstring))
  (command-line pointer :key)
  (creation-flags process-creation-flags)
  (environment pointer :key)
  (current-directory (& wstring :in t) :key)
  (startup-info (& (union ()
                          (info* startup-info*)
                          (info startup-info)))
                :key (make-startup-info))
  (process-info (& process-information :out) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("CreateProcessWithTokenW"
      create-process-with-token)
    (:stdcall advapi32)
  ((last-error bool) rv process-info)
  "Creates a new process and its primary thread. The new process runs in the security context of the specified token. It can optionally load the user profile for the specified user."
  (token handle)
  (logon-flags (enum (:base-type dword)
                 (:with-profile #x00000001)
                 (:net-credentials-only #x00000002)))
  (application-name (& wstring))
  (command-line pointer :key)
  (creation-flags process-creation-flags)
  (environment pointer :key)
  (current-directory (& wstring :in t) :key)
  (startup-info (& (union ()
                          (info* startup-info*)
                          (info startup-info)))
                :key (make-startup-info))
  (process-info (& process-information :out) :aux))

(define-external-function
    ("ExitProcess" (:camel-case))
    (:stdcall kernel32)
  (void)
  "Ends the calling process and all its threads."
  (exit-code uint :optional 0))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("FlushProcessWriteBuffers" (:camel-case))
    (:stdcall kernel32)
  (void)
  "Flushes the write queue of each processor that is running a thread of the current process.")

(define-external-function
    (#+doors.unicode "FreeEnvironmentStringsW"
     #-doors.unicode "FreeEnvironmentStringsA"
                   free-environment-strings)
    (:stdcall kernel32)
  ((last-error bool))
  "Frees a block of environment strings."
  (environment-block pointer))

(define-external-function
    (#+doors.unicode "GetCommandLineW"
     #-doors.unicode "GetCommandLineA"
                   command-line)
    (:stdcall kernel32)
  ((& tstring))
  "Retrieves the command-line string for the current process.")

(define-symbol-macro command-line (command-line))

(define-external-function
    ("GetCurrentProcess" current-process)
    (:stdcall kernel32)
  (handle)
  "Retrieves a pseudo handle for the current process.")

(define-symbol-macro current-process (current-process))

(define-external-function
    ("GetCurrentProcessId" current-process-id)
    (:stdcall kernel32)
  (dword)
  "Retrieves the process identifier of the calling process.")

(define-symbol-macro current-process-id (current-process-id))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("GetCurrentProcessorNumber" current-processor-number)
    (:stdcall kernel32)
  (dword)
  "Retrieves the number of the processor the current thread was running on during the call to this function.")

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-symbol-macro current-processor-number (current-processor-number))

(define-external-function
    (#+doors.unicode "GetEnvironmentStringsW"
     #-doors.unicode "GetEnvironmentStringsA"
                   environment-strings)
    (:stdcall kernel32)
  (pointer)
  "Retrieves the environment variables for the current process.")

(define-symbol-macro environment-strings (environment-strings))

(define-external-function
    (#+doors.unicode "GetEnvironmentVariableW"
     #-doors.unicode "GetEnvironmentVariableA"
                   environment-variable)
    (:stdcall kernel32)
  ((last-error dword not-zero) rv
   (external-function-call
     #+doors.unicode "GetEnvironmentVariableW"
     #-doors.unicode "GetEnvironmentVariableA"
     ((:stdcall kernel32)
      ((last-error dword not-zero) rv (subseq buffer 0 rv))
      ((& tstring) name :aux name)
      ((& tstring :out) buffer :aux (make-string rv))
      (dword size :aux rv))))
  "Retrieves the contents of the specified variable from the environment block of the calling process."
  (name (& tstring))
  (buffer pointer :aux)
  (size dword :aux))

(define-external-function
    ("GetExitCodeProcess" process-exit-code)
    (:stdcall kernel32)
  ((last-error bool) rv
   (if (= code 259)
     nil
     code))
  "Retrieves the termination status of the specified process."
  (process handle)
  (code (& dword :out) :aux))

(define-external-function
    ("GetGuiResources" gui-resources)
    (:stdcall user32)
  ((last-error dword not-zero))
  "Retrieves the count of handles to graphical user interface (GUI) objects in use by the specified process."
  (process handle :optional current-process)
  (flags (enum (:base-type dword)
               :gdi-objects
               :user-objects
               :gdi-objects-peak
               (:user-objects-peak 4))))

(define-enum (priority-class
               (:base-type dword)
               (:conc-name nil))
  (:above-normal-priority-class #x00008000)
  (:below-normal-priority-class #x00004000)
  (:high-priority-class #x00000080)
  (:idle-priority-class #x00000040)
  (:normal-priority-class #x00000020)
  (:realtime-priority-class #x00000100))

(define-external-function
    ("GetPriorityClass" priority-class)
    (:stdcall kernel32)
  (dword rv (if (zerop rv)
              (invoke-last-error)
              (translate rv 'priority-class)))
  "Retrieves the priority class for the specified process. "
  (process handle :optional current-process))

(define-symbol-macro priority-class (priority-class))

(define-external-function
    ("GetProcessAffinityMask" process-affinity-mask)
    (:stdcall kernel32)
  ((last-error bool) rv (values process-affinity-mask
                                system-affinity-mask))
  "Retrieves the process affinity mask for the specified process and the system affinity mask for the system."
  (process handle :optional current-process)
  (process-affinity-mask (& uint-ptr :out) :aux)
  (system-affinity-mask (& uint-ptr :out) :aux))

(define-symbol-macro process-affinity-mask (process-affinity-mask))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver winvista winserver2008)
(define-external-function
    ("GetProcessGroupAffinity" process-group-affinity)
    (:stdcall kernel32)
  (bool rv
        (if rv
          (make-array 0 :element-type 'ushort
            :initial-element 0)
          (let ((last-error-code (hresult-from-win32 last-error)))
            (if (= last-error-code error-insufficient-buffer)
              (external-function-call "GetProcessGroupAffinity"
                ((:stdcall kernel32)
                 ((last-error bool) rv (subseq group-array 0 group-count))
                 (handle hprocess :aux process)
                 ((& ushort :inout) group-count :aux count)
                 ((& (simple-array ushort) :out) group-array :aux
                  (make-array count :element-type 'ushort
                    :initial-element 0))))
              (error 'windows-error :code (hresult-from-win32
                                           last-error-code))))))
  "Retrieves the processor group affinity of the specified process."
  (process handle :optional current-process)
  (count (& ushort :inout) :aux 0)
  (array (& ushort :out) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver winvista winserver2008)
(define-symbol-macro process-group-affinity (process-group-affinity))

#-win2000
(define-external-function
    ("GetProcessHandleCount" process-handle-count)
    (:stdcall kernel32)
  ((last-error bool) rv count)
  "Retrieves the number of open handles that belong to the specified process."
  (process handle :optional current-process)
  (count (& dword :out) :aux))

#-win2000
(define-symbol-macro process-handle-count (process-handle-count))

#-win2000
(define-external-function
    ("GetProcessId" process-id)
    (:stdcall kernel32)
  ((last-error dword not-zero))
  "Retrieves the process identifier of the specified process."
  (process handle :optional current-process))

#-win2000
(define-symbol-macro process-id (process-id))

#-(or win2000 winxp winhomeserver winxp64)
(define-external-function
    ("GetProcessIdOdThread" process-id-of-thread)
    (:stdcall kernel32)
  ((last-error dword not-zero))
  "Retrieves the process identifier of the process associated with the specified thread."
  (thread handle :optional (external-function-call
                             "GetCurrentThread"
                             ((:stdcall kernel32)
                              (handle)))))

#-(or win2000 winxp winhomeserver winxp64)
(define-symbol-macro process-id-of-thread (process-id-of-thread))

(define-external-function
    ("GetProcessIoCounters" process-io-counters)
    (:stdcall kernel32)
  ((last-error bool) rv counters)
  "Retrieves accounting information for all I/O operations performed by the specified process."
  (process handle :optional current-process)
  (counters (& io-counters :out) :aux))

(define-symbol-macro process-io-counters (process-io-counters))

(define-external-function
    ("GetProcessPriorityBoost" process-priority-boost)
    (:stdcall kernel32)
  ((last-error bool) rv boost)
  "Retrieves the priority boost control state of the specified process."
  (process handle :optional current-process)
  (boost (& bool :out) :aux))

(define-symbol-macro process-priority-boost (process-priority-boost))

(define-external-function
    ("GetProcessShutdownParameters" process-shutdown-parameters)
    (:stdcall kernel32)
  ((last-error bool) rv (values level flags))
  "Retrieves the shutdown parameters for the currently calling process."
  (level (& dword :out) :aux)
  (flags (& (enum (:base-type dword)
              (:no-retry #x00000001))
          :out)
         :aux))

(define-symbol-macro process-shutdown-parameters
    (process-shutdown-parameters))

(define-external-function
    ("GetProcessTimes" process-times)
    (:stdcall kernel32)
  ((last-error bool) rv (values creation-time exit-time kernel-time user-time))
  "Retrieves timing information for the specified process."
  (process handle :optional current-process)
  (creation-time (& file-time :out) :aux)
  (exit-time (& file-time :out) :aux)
  (kernel-time (& file-time :out) :aux)
  (user-time (& file-time :out) :aux))

(define-symbol-macro process-times (process-times))

(define-external-function
    ("GetProcessVersion" process-version)
    (:stdcall kernel32)
  ((last-error dword not-zero))
  "Retrieves the major and minor version numbers of the system on which the specified process expects to run."
  (process-id dword :optional 0))

(define-symbol-macro process-version (process-version))

(define-external-function
    ("GetProcessWorkingSetSize" process-working-set-size)
    (:stdcall kernel32)
  ((last-error bool) rv (values min max))
  "Retrieves the minimum and maximum working set sizes of the specified process."
  (process handle :optional current-process)
  (min (& dword :out) :aux)
  (max (& dword :out) :aux))

(define-symbol-macro process-working-set-size (process-working-set-size))

#-(or win2000 winxp winhomeserver winxp64)
(define-external-function
    ("GetProcessWorkingSetSizeEx" process-working-set-size*)
    (:stdcall kernel32)
  ((last-error bool) rv (values min max flags))
  "Retrieves the minimum and maximum working set sizes of the specified process."
  (process handle :optional current-process)
  (min (& size-t :out) :aux)
  (max (& size-t :out) :aux)
  (flags (& (enum (:base-type dword :list t)
              (:min-disable #x00000002)
              (:min-enable  #x00000001)
              (:max-disable #x00000008)
              (:max-enable  #x00000004))
          :out)
         :aux))

#-(or win2000 winxp winhomeserver winxp64)
(define-symbol-macro process-working-set-size*
    (process-working-set-size*))

#-(or win2000 winxp winhomeserver winxp64 winserver2003 winvista winserver2008)
(define-external-function
    ("GetProcessorSystemCycleTime" processor-system-cycle-time)
    (:stdcall kernel32)
  ((last-error bool) rv buffer)
  "Retrieves the cycle time each processor in the specified processor group spent executing deferred procedure calls (DPCs) and interrupt service routines (ISRs) since the processor became active."
  (group ushort)
  (buffer (& qword :out) :aux buffer)
  (length (& dword :out) :aux (sizeof 'qword)))

(define-external-function
    (#+doors.unicode "GetStartupInfoW"
     #-doors.unicode "GetStartupInfoA"
                   startup-info)
    (:stdcall kernel32)
  (void rv info)
  "Retrieves the contents of the startup-info structure that was specified when the calling process was created."
  (info (& startup-info :out) :aux))

(define-symbol-macro startup-info (startup-info))

#-(or win2000 winxp winhomeserver winxp64)
(define-external-function
    (#+doors.unicode "NeedCurrentDirectoryForExePathW"
     #-doors.unicode "NeedCurrentDirectoryForExePathA"
                   need-current-directory-for-exe-path)
    (:stdcall kernel32)
  (bool)
  "Determines whether the current directory should be included in the search path for the specified executable."
  (path (& tstring)))

(define-enum (process-access-flags
               (:base-type dword)
               (:list t)
               (:conc-name process-access-))
  (:delete #x00010000)
  (:read-control #x00020000)
  (:synchronize #x00100000)
  (:write-dac #x00040000)
  (:write-owner #x00080000)
  (:all #x001F0FFF)
  (:create-process #x0080)
  (:create-thread #x0002)
  (:dup-handle #x0040)
  (:query-information #x0400)
  (:query-limited-information #x1000)
  (:set-information #x0200)
  (:set-quota #x0100)
  (:suspend-resume #x0800)
  (:terminate #x0001)
  (:vm-operation #x0008)
  (:vm-read #x0010)
  (:vm-write #x0020))

(define-external-function
    ("OpenProcess" (:camel-case))
    (:stdcall kernel32)
  ((last-error handle))
  "Opens an existing local process object."
  (desired-access process-access-flags)
  (inherit-handle boolean :optional)
  (process-id dword))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    (#+doors.unicode "QueryFullProcessImageNameW"
     #-doors.unicode "QueryFullProcessImageNameA"
                   full-process-image-name)
    (:stdcall kernel32)
  ((last-error bool) rv (subseq name 0 size))
  "Retrieves the full name of the executable image for the specified process."
  (process handle :optional current-process)
  (flags (enum (:base-type dword) (:native-name 1)) :optional)
  (name (& tstring :out) :aux (make-string size))
  (size (& dword :inout) :optional 256))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-symbol-macro full-process-image-name
    (full-process-image-name))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("QueryProcessAffinityUpdateMode" process-affinity-update-mode)
    (:stdcall kernel32)
  ((last-error bool) rv flags)
  "Retrieves the affinity update mode of the specified process."
  (process handle :optional current-process)
  (flags (& (enum (:base-type dword)
                  :disable-auto-update
                  :enable-auto-update)
          :out)
         :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-symbol-macro process-affinity-update-mode
    (process-affinity-update-mode))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("QueryProcessCycleTime" process-cycle-time)
    (:stdcall kernel32)
  ((last-error bool) rv cycle-time)
  "Retrieves the sum of the cycle time of all threads of the specified process."
  (process handle :optional current-process)
  (cycle-time (& qword :out) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-symbol-macro process-cycle-time (process-cycle-time))

(define-external-function
    (#+doors.unicode "SetEnvironmentVariableW"
     #-doors.unicode "SetEnvironmentVariableA"
                   (setf environment-variable))
    (:stdcall kernel32)
  ((last-error bool) rv value)
  "Sets the contents of the specified environment variable for the current process."
  (name (& tstring) :optional)
  (value (& tstring :in t)))

(define-external-function
    ("SetPriorityClass" (setf priority-class))
    (:stdcall kernel32)
  ((last-error bool) rv priority-class)
  "Sets the priority class for the specified process. "
  (process handle :optional current-process)
  (priority-class priority-class))

(define-external-function
    ("SetProcessAffinityMask" (setf process-affinity-mask))
    (:stdcall kernel32)
  ((last-error bool) rv mask)
  "Sets a processor affinity mask for the threads of the specified process."
  (process handle :optional current-process)
  (mask uint-ptr))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("SetProcessAffinityUpdateMode" (setf process-affinity-update-mode))
    (:stdcall kernel32)
  ((last-error bool) rv flags)
  "Sets the affinity update mode of the specified process."
  (process handle :optional current-process)
  (flags (enum (:base-type dword)
               :disable-auto-update
               :enable-auto-update)))

(define-external-function
    ("SetProcessPriorityBoost" (setf process-priority-boost))
    (:stdcall kernel32)
  ((last-error bool) rv disable-boost)
  "Disables or enables the ability of the system to temporarily boost the priority of the threads of the specified process."
  (process handle :optional current-process)
  (disable-boost boolean))

(define-external-function
    ("SetProcessShutdownParameters" (setf process-shutdown-parameters))
    (:stdcall kernel32)
  ((last-error bool) rv (values level flags))
  "Sets shutdown parameters for the currently calling process. "
  (level dword)
  (flags (enum (:base-type dword) (:no-retry 1)) :optional))

(define-external-function
    ("SetProcessWorkingSetSize" (setf process-working-set-size))
    (:stdcall kernel32)
  ((last-error bool) rv (values min max))
  "Sets the minimum and maximum working set sizes for the specified process."
  (process handle)
  (min size-t :optional #+x86-64 #xFFFFFFFFFFFFFFFF
                        #-x86-64 #xFFFFFFFF)
  (max size-t :optional #+x86-64 #xFFFFFFFFFFFFFFFF
                        #-x86-64 #xFFFFFFFF))

#-(or win2000 winxp winxp64 winhomeserver)
(define-external-function
    ("SetProcessWorkingSetSizeEx" (setf process-working-set-size*))
    (:stdcall kernel32)
  ((last-error bool) rv (values min max flags))
  "Sets the minimum and maximum working set sizes for the specified process."
  (process handle)
  (min size-t :optional #+x86-64 #xFFFFFFFFFFFFFFFF
                        #-x86-64 #xFFFFFFFF)
  (max size-t :optional #+x86-64 #xFFFFFFFFFFFFFFFF
                        #-x86-64 #xFFFFFFFF)
  (flags (enum (:base-type dword :list t)
              (:min-disable #x00000002)
              (:min-enable  #x00000001)
              (:max-disable #x00000008)
              (:max-enable  #x00000004))
         :optional))

(define-external-function
    ("TerminateProcess" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool))
  "Terminates the specified process and all of its threads."
  (process handle :optional current-process)
  (exit-code uint))
