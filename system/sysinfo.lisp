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
        (dword size rv))))
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
