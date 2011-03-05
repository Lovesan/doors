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

(define-enum (dock-info
               (:base-type dword))  
  (:undocked      1)
  (:docked        2)
  (:user-supplied 4)  
  (:user-undocked 5)
  (:user-docked   6))

(define-struct (hw-profile-info
                 (:conc-name hw-))
    "Contains information about a hardware profile."
  (dock-info dock-info)
  (profile-guid (tstring 39))
  (profile-name (tstring 80)))

(define-enum (processor-architecture
               (:base-type dword))
  (:intel 0)
  (:amd64 9)
  (:ia64  6)
  (:unknown #xffff))

(define-enum (processor-type
               (:conc-name processor-)
               (:base-type dword))
  (:intel-386     386)
  (:intel-486     486)
  (:intel-pentium 586)
  (:intel-ia64    2200)
  (:amd-x86-64    8664))

(define-struct (system-info
                 (:conc-name sysinfo-))
    "Contains information about the current computer system."
  (processor-architecture processor-architecture)
  (page-size dword)
  (minimum-application-address pointer)
  (maximum-application-address pointer)
  (active-processor-mask uint-ptr)
  (number-of-processors dword)
  (processor-type processor-type)
  (allocation-granularity dword)
  (processor-level (enum (:base-type word)
                     (:ia64 1)))
  (processor-revision word))

(define-enum (computer-name-format
               (:conc-name computer-name-))
  :netbios
  :dns-hostname
  :dns-domain
  :dns-fully-qualified
  :physical-netbios
  :physical-dns-hostname
  :physical-dns-domain
  :physical-dns-fully-qualified)

(define-enum (extended-name-format
               (:conc-name name-))
  :unknown
  :fully-qualified-dn
  :sam-compatible
  :display
  (:unique-id 6)
  :canonical
  :user-principal
  :canonical*
  :service-principal
  (:dns-domain 12))

(define-external-function
    (#+doors.unicode "DnsHostnameToComputerNameW"
     #-doors.unicode "DnsHostnameToComputerNameA"
                   dns-hostname-to-computer-name)
    (:stdcall kernel32)
  (bool rv
        (let ((error-code (hresult-from-win32 last-error)))
          (if (= error-code error-more-data)
            (external-function-call
              #+doors.unicode "DnsHostnameToComputerNameW"
              #-doors.unicode "DnsHostnameToComputerNameA"
              ((:stdcall kernel32)
               ((last-error bool) rv (if (= size %size)
                                       buffer
                                       (subseq buffer 0 size)))
               ((& tstring) %hostname :aux hostname)
               ((& tstring :out) buffer :aux (make-string %size))
               ((& dword :inout) size :aux %size)))
            (error 'windows-error :code error-code))))
  "Converts a DNS-style host name to a NetBIOS-style computer name."
  (hostname (& tstring))
  (%buffer pointer :aux &0)
  (%size (& dword :inout) :aux 0))

#-(or win2000 winxp winxp64 winhomeserver)
(define-external-function
    ("EnumSystemFirmwareTables" (:camel-case))
    (:stdcall kernel32)
  ((last-error uint not-zero) rv
     (external-function-call
       "EnumSystemFirmwareTables"
       ((:stdcall kernel32)
        ((last-error uint not-zero) rv buffer)
        (dword signature :aux firmware-table-provider-signature)
        ((& (simple-array dword) :out) buffer
         :aux (make-array rv :element-type 'dword))
        (dword size :aux rv))))
  "Enumerates all system firmware tables of the specified type."
  (firmware-table-provider-signature dword)
  (%buffer pointer :aux &0)
  (size dword :aux 0))

(define-external-function
    (#+doors.unicode "ExpandEnvironmentStringsW"
     #-doors.unicode "ExpandEnvironmentStringsA"
                   expand-environment-strings)
    (:stdcall kernel32)
  ((last-error dword not-zero) len
   (external-function-call
     #+doors.unicode "ExpandEnvironmentStringsW"
     #-doors.unicode "ExpandEnvironmentStringsA"
     ((:stdcall kernel32)
      ((last-error dword not-zero) rv (subseq dest 0 (1- rv)))
      ((& tstring) src :aux source)
      ((& tstring :out) dest :aux (make-string (+ 1 len)))
      (dword size :aux (1+ len)))))
  "Expands environment-variable strings and replaces them with the values defined for the current user."
  (source (& tstring))
  (%dest pointer :aux &0)
  (%size dword :aux 0))

(define-external-function
    (#+doors.unicode "GetComputerNameW"
     #-doors.unicode "GetComputerNameA"
                   computer-name)
    (:stdcall kernel32)
  (bool rv (let ((errcode (hresult-from-win32 last-error)))
             (if (= errcode error-buffer-overflow)
               (external-function-call
                 #+doors.unicode "GetComputerNameW"
                 #-doors.unicode "GetComputerNameA"
                 ((:stdcall kernel32)
                  ((last-error bool) rv (subseq buffer 0 size))
                  ((& tstring :out) buffer :aux (make-string (1+ %size)))
                  ((& dword :inout) size :aux (1+ %size))))
               (error 'windows-error :code errcode))))
  "Retrieves the NetBIOS name of the local computer."
  (%buffer pointer :aux &0)
  (%size (& dword :inout) :aux 0))

(define-symbol-macro computer-name (computer-name))

(define-external-function
    (#+doors.unicode "GetComputerNameExW"
     #-doors.unicode "GetComputerNameExA"
                   computer-name*)
    (:stdcall kernel32)
  (bool rv (let ((errcode (hresult-from-win32 last-error)))
             (if (= errcode error-more-data)
               (external-function-call
                 #+doors.unicode "GetComputerNameExW"
                 #-doors.unicode "GetComputerNameExA"
                 ((:stdcall kernel32)
                  ((last-error bool) rv (subseq buffer 0 size))
                  (computer-name-format nformat :aux name-format)
                  ((& tstring :out) buffer :aux (make-string (1+ %size)))
                  ((& dword :inout) size :aux (1+ %size))))
               (error 'windows-error :code errcode))))
  "Retrieves a NetBIOS or DNS name associated with the local computer."
  (name-format computer-name-format)
  (%buffer pointer :aux &0)
  (%size (& dword :inout) :aux 0))

(define-external-function
    (#+doors.unicode "GetComputerObjectNameW"
     #-doors.unicode "GetComputerObjectNameA"
                   computer-object-name)
    (:stdcall secur32)
  (bool rv (let ((errcode (hresult-from-win32 last-error)))
             (if (= errcode error-insufficient-buffer)
               (external-function-call
                 #+doors.unicode "GetComputerObjectNameW"
                 #-doors.unicode "GetComputerObjectNameA"
                 ((:stdcall secur32)
                  ((last-error bool) rv (subseq buffer 0 size))
                  (extended-name-format nformat :aux name-format)
                  ((& tstring :out) buffer :aux (make-string (1+ %size)))
                  ((& dword :inout) size :aux (1+ %size))))
               (error 'windows-error :code errcode))))
  "Retrieves the local computer's name in a specified format."
  (name-format extended-name-format)
  (%buffer pointer :aux &0)
  (%size (& dword :inout) :aux 0))

(define-external-function
    (#+doors.unicode "GetCurrentHwProfileW"
     #-doors.unicode "GetCurrentHwProfileA"
                   current-hw-profile)
    (:stdcall advapi32)
  ((last-error bool) rv info)
  "Retrieves information about the current hardware profile for the local computer."
  (info (& hw-profile-info :out) :aux))

(define-symbol-macro current-hw-profile (current-hw-profile))

#-win2000
(define-external-function
    (#+doors.unicode "GetFirmwareEnvironmentVariableW"
     #-doors.unicode "GetFirmwareEnvironmentVariableA"
                   firmware-environment-variable)
    (:stdcall kernel32)
  ((last-error dword not-zero) rv)
  "Retrieves the value of the specified firmware environment variable."
  (name (& tstring))
  (guid (& tstring))
  (buffer pointer)
  (buffer-size dword))

#-win2000
(define-external-function
    ("GetNativeSystemInfo" native-system-info)
    (:stdcall kernel32)
  (void rv info)
  "Retrieves information about the current system to an application running under WOW64."
  (info (& system-info :out) :aux))

#-win2000
(define-symbol-macro native-system-info (native-system-info))

(define-enum (product-type
               (:base-type dword)
               (:conc-name product-))
  (:business #x00000006)
  (:business-n #x00000010)
  (:cluster-server #x00000012)
  (:datacenter-server #x00000008)
  (:datacenter-server-core #x0000000C)
  (:datacenter-server-core-v #x00000027)
  (:datacenter-server-v #x00000025)
  (:enterprise #x00000004)
  (:enterprise-n #x0000001B)
  (:enterprise-server #x0000000A)
  (:enterprise-server-core #x0000000E)
  (:enterprise-server-core-v #x00000029)
  (:enterprise-server-ia64 #x0000000F)
  (:enterprise-server-v #x00000026)
  (:home-basic #x00000002)
  (:home-basic-n #x00000005)
  (:home-premium #x00000003)
  (:home-premium-n #x0000001A)
  (:hyper-v #x0000002A)
  (:medium-business-server-management #x0000001E)
  (:medium-business-server-messaging #x00000020)
  (:medium-business-server-security #x0000001F)
  (:professional #x00000030)
  (:professional-n #x00000031)
  (:server-for-small-business #x00000018)
  (:server-for-small-business-v #x00000023)
  (:server-foundation #x00000021)
  (:small-business-server #x00000009)
  (:solution-embedded-server #x00000038)
  (:standard-server #x00000007)
  (:standard-server-core #x0000000D)
  (:standard-server-core-v #x00000028)
  (:standard-server-v #x00000024)
  (:starter #x0000000B)
  (:starter-n #x0000002F)
  (:storage-enterprise-server #x00000017)
  (:storage-express-server #x00000014)
  (:storage-standard-server #x00000015)
  (:storage-workgroup-server #x00000016)
  (:undefined #x00000000)
  (:ultimate #x00000001)
  (:ultimate-n #x0000001C)
  (:web-server #x00000011)
  (:web-server-core #x0000001D))

#-(or win2000 winxp winxp64 winhomeserver winserver2003)
(define-external-function
    ("GetProductInfo" product-info)
    (:stdcall kernel32)
  ((last-error bool) rv product-type)
  "Retrieves the product type for the operating system on the local computer."
  (os-major dword)
  (os-minor dword)
  (sp-major dword)
  (sp-minor dword)
  (product-type (& product-type :out) :aux))

(define-external-function
    (#+doors.unicode "GetSystemDirectoryW"
     #-doors.unicode "GetSystemDirectoryA"
                   system-directory)
    (:stdcall kernel32)
  ((last-error uint not-zero) rv
   (external-function-call
     #+doors.unicode "GetSystemDirectoryW"
     #-doors.unicode "GetSystemDirectoryA"
     ((:stdcall kernel32)
      ((last-error uint not-zero) %rv buffer)
      ((& tstring :out) buffer :aux (make-string (1- rv)))
      (uint size :aux rv))))
  "Retrieves the path of the system directory."
  (%buffer pointer :aux &0)
  (%size dword :aux 0))

(define-symbol-macro system-directory (system-directory))

#-(or win2000 winxp winhomeserver)
(define-external-function
    ("GetSystemFirmwareTable" system-firmware-table)
    (:stdcall kernel32)
  ((last-error uint not-zero) rv (values
                                   rv
                                   (<= rv buffer-size)))
  "Retrieves the specified firmware table from the firmware table provider."
  (table-provider-signature dword)
  (table-id dword)
  (firmware-table-buffer pointer)
  (buffer-size dword))

(define-external-function
    ("GetSystemInfo" system-info)
    (:stdcall kernel32)
  (void rv info)
  "Retrieves information about the current system."
  (info (& system-info :out) :aux))

(define-symbol-macro system-info (system-info))

#-win2000
(define-external-function
    ("GetSystemRegistryQuota" system-registry-quota)
    (:stdcall kernel32)
  ((last-error bool) rv (values allowed used))
  "Retrieves the current size of the registry and the maximum size that the registry is allowed to attain on the system."
  (allowed (& dword :out) :aux)
  (used (& dword :out) :aux))

#-win2000
(define-symbol-macro system-registry-quota (system-registry-quota))

(define-external-function
    (#+doors.unicode "GetSystemWindowsDirectoryW"
     #-doors.unicode "GetSystemWindowsDirectoryA"
                   system-windows-directory)
    (:stdcall kernel32)
  ((last-error uint not-zero) rv
   (external-function-call
     #+doors.unicode "GetSystemWindowsDirectoryW"
     #-doors.unicode "GetSystemWindowsDirectoryA"
     ((:stdcall kernel32)
      ((last-error uint not-zero) %rv buffer)
      ((& tstring :out) buffer :aux (make-string (1- rv)))
      (uint size :aux rv))))
  "Retrieves the path of the shared Windows directory on a multi-user system."
  (%buffer pointer :aux &0)
  (%size dword :aux 0))

(define-symbol-macro system-windows-directory (system-windows-directory))

(define-external-function
    (#+doors.unicode "GetWindowsDirectoryW"
     #-doors.unicode "GetWindowsDirectoryA"
                   windows-directory)
    (:stdcall kernel32)
  ((last-error uint not-zero) rv
   (external-function-call
     #+doors.unicode "GetWindowsDirectoryW"
     #-doors.unicode "GetWindowsDirectoryA"
     ((:stdcall kernel32)
      ((last-error uint not-zero) %rv buffer)
      ((& tstring :out) buffer :aux (make-string (1- rv)))
      (uint size :aux rv))))
  "Retrieves the path of the Windows directory."
  (%buffer pointer :aux &0)
  (%size dword :aux 0))

(define-symbol-macro windows-directory (windows-directory))

#-win2000
(define-external-function
    (#+doors.unicode "GetSystemWow64DirectoryW"
     #-doors.unicode "GetSystemWow64DirectoryA"
                   system-wow64-directory)
    (:stdcall kernel32)
  ((last-error uint not-zero) rv
   (external-function-call
     #+doors.unicode "GetSystemWow64DirectoryW"
     #-doors.unicode "GetSystemWow64DirectoryA"
     ((:stdcall kernel32)
      ((last-error uint not-zero) %rv buffer)
      ((& tstring :out) buffer :aux (make-string (1- rv)))
      (uint size :aux rv))))
  "Retrieves the path of the system directory used by WOW64."
  (%buffer pointer :aux &0)
  (%size dword :aux 0))

#-win2000
(define-symbol-macro system-wow64-directory (system-wow64-directory))

(define-external-function
    (#+doors.unicode "GetUserNameW"
     #-doors.unicode "GetUserNameA"
                   user-name)
    (:stdcall advapi32)
  (bool rv (let ((errcode (hresult-from-win32 last-error)))
             (if (= errcode error-insufficient-buffer)
               (external-function-call
                 #+doors.unicode "GetUserNameW"
                 #-doors.unicode "GetUserNameA"
                 ((:stdcall advapi32)
                  ((last-error bool) rv (if (/= size %size)
                                          (subseq buffer 0 size)
                                          buffer))
                  ((& tstring :out) buffer :aux (make-string (1- %size)))
                  ((& dword :inout) size :aux %size)))
               (error 'windows-error :code errcode))))
  "Retrieves the name of the user associated with the current thread."
  (%buffer pointer :aux)
  (%size (& dword :inout) :aux 0))

(define-symbol-macro user-name (user-name))

(define-external-function
    (#+doors.unicode "GetUserNameExW"
     #-doors.unicode "GetUserNameExA"
                   user-name*)
    (:stdcall secur32)
  (bool rv (let ((errcode (hresult-from-win32 last-error)))
             (if (= errcode error-more-data)
               (external-function-call
                 #+doors.unicode "GetUserNameExW"
                 #-doors.unicode "GetUserNameExA"
                 ((:stdcall secur32)
                  ((last-error bool) rv (if (/= size %size)
                                          (subseq buffer 0 size)
                                          buffer))
                  (extended-name-format frmt :aux name-format)
                  ((& tstring :out) buffer :aux (make-string (1- %size)))
                  ((& dword :inout) size :aux %size)))
               (error 'windows-error :code errcode))))
  "Retrieves the name of the user or other security principal associated with the calling thread."
  (name-format extended-name-format)
  (%buffer pointer :aux)
  (%size (& dword :inout) :aux 0))

(define-enum (processor-features
               (:base-type dword)
               (:conc-name pf-))
  (:3dnow-instructions-available 7)
  (:channels-enabled 16)
  (:compare-exchange-double 2)
  (:compare-exchange-128 14)
  (:compare-64-exchange-128 15)
  (:floating-point-emulated 1)
  (:floating-point-precision-errata 0)
  (:mmx-instructions-available 3)
  (:nx-enabled 12)
  (:pae-enabled 9)
  (:rdtsc-instruction-available 8)
  (:sse3-instructions-available 13)
  (:xmmi-instructions-available 6)
  (:xmmi64-instructions-available 10)
  (:xsave-enabled 17))

(define-external-function
    ("IsProcessorFeaturePresent" (:camel-case))
    (:stdcall kernel32)
  (bool)
  "Determines whether the specified processor feature is supported by the current computer."
  (processor-feature processor-features))

(define-external-function
    (#+doors.unicode "SetComputerNameW"
     #-doors.unicode "SetComputerNameA"
                   (setf computer-name))
    (:stdcall kernel32)
  ((last-error bool) rv name)
  "Sets a new NetBIOS name for the local computer."
  (name (& tstring)))

(define-external-function
    (#+doors.unicode "SetComputerNameExW"
     #-doors.unicode "SetComputerNameExA"
                   (setf computer-name*))
    (:stdcall kernel32)
  ((last-error bool) rv (values name name-type))
  "Sets a new NetBIOS or DNS name for the local computer."
  (name-type computer-name-format :optional :physical-netbios)
  (name (& tstring)))

#-win2000
(define-external-function
    (#+doors.unicode "SetFirmwareEnvironmentVariableW"
     #-doors.unicode "SetFirmwareEnvironmentVariableA"
                   (setf firmware-environment-variable))
    (:stdcall kernel32)
  ((last-error bool) rv (values buffer size))
  "Sets the value of the specified firmware environment variable."
  (name (& tstring) :optional "")
  (guid (& tstring) :optional "{00000000-0000-0000-0000-000000000000}")
  (buffer pointer)
  (size dword))

(define-external-function
    (#+doors.unicode "TranslateNameW"
     #-doors.unicode "TranslateNameA"
                   translate-account-name)
    (:stdcall secur32)
  ((last-error bool) rv
     (external-function-call
       #+doors.unicode "TranslateNameW"
       #-doors.unicode "TranslateNameA"
       ((:stdcall secur32)
        ((last-error bool) rv (if (/= size %size)
                                (subseq buffer 0 size)
                                buffer))
        ((& tstring))
        (extended-name-format)
        (extended-name-format)
        ((& tstring :out) buffer :aux (make-string (1- %size)))
        ((& dword :inout) size :aux %size))
       account-name
       account-name-format
       desired-name-format))
  "Converts a directory service object name from one format to another."
  (account-name (& tstring))
  (account-name-format extended-name-format)
  (desired-name-format extended-name-format)
  (%buffer pointer :aux &0)
  (%size (& ulong :inout) :aux 0))

(define-external-function
    (#+doors.unicode "VerifyVersionInfoW"
     #-doors.unicode "VerifyVersionInfoA"
                   verify-version-info)
    (:stdcall kernel32)
  ((last-error bool))
  "Compares a set of operating system version requirements to the corresponding values for the currently running version of the system."
  (version-info (& os-version-info))
  (type-mask (enum (:base-type dword :list t)
               (:build-number 4)
               (:major-version 2)
               (:minor-version 1)
               (:platform-id 8)
               (:service-pack-major #x20)
               (:service-pack-minor #x10)
               (:suite-name #x40)
               (:product-type #x80)))
  (condition-mask qword))

(define-external-function
    ("VerSetConditionMask" (setf ver-condition-mask))
    (:stdcall kernel32)
  (qword)
  "Sets the bits of a 64-bit value to indicate the comparison operator to use for a specified operating system version attribute."
  (mask qword)
  (type-mask (enum (:base-type dword :list t)
               (:build-number 4)
               (:major-version 2)
               (:minor-version 1)
               (:platform-id 8)
               (:service-pack-major #x20)
               (:service-pack-minor #x10)
               (:suite-name #x40)
               (:product-type #x80)))
  (condition-mask (enum (:base-type byte :list t)
                    (:equal 1)
                    :greater
                    :greater-equal
                    :less
                    :less-equal
                    :and
                    :or)))
