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

(define-enum (registry-rights
               (:base-type dword)
               (:conc-name key))
  (:all-access #xF003F)
  (:create-link #x0020)
  (:create-sub-key #x0004)
  (:enumerate-sub-keys #x0008)
  (:execute #x20019)
  (:notify #x0010)
  (:query-value #x0001)
  (:read #x20019)
  (:set-value #x0002)
  (:wow64-32-key #x0200)  
  (:wow64-64-key #x0100)
  (:write #x20006)
  (:delete #x10000)
  (:read-control #x20000)
  (:write-owner #x80000)
  (:write-dac #x40000))

(define-symbol-macro hkey-classes-root          (& #x80000000))
(define-symbol-macro hkey-current-user          (& #x80000001))
(define-symbol-macro hkey-local-machine         (& #x80000002))
(define-symbol-macro hkey-users                 (& #x80000003))
(define-symbol-macro hkey-performance-data      (& #x80000004))
(define-symbol-macro hkey-performance-text      (& #x80000050))
(define-symbol-macro hkey-performance-nls-text  (& #x80000060))
(define-symbol-macro hkey-current-config        (& #x80000005))
(define-symbol-macro hkey-dyn-data              (& #x80000006))

(define-enum (registry-value-type
               (:conc-name reg-)
               (:base-type dword))
  :none
  :sz
  :expand-sz
  :binary
  :dword
  :dword-big-endian
  :link
  :multi-sz
  :resource-list
  :full-resource-descriptor
  :resource-requirements-list
  :qword)

(define-enum (registry-type-restrictions
               (:base-type dword)
               (:list t)
               (:conc-name reg-rt-))
  (:none #x00000001)
  (:any  #x0000FFFF)
  (:sz   #x00000002)
  (:expand-sz #x00000004)
  (:binary #x00000008)
  (:dword #x00000010)
  (:multi-sz #x00000020)
  (:qword #x00000040)
  (:no-expand #x10000000)
  (:zero-on-failure #x20000000))

(define-external-function
    (#+doors.unicode "RegOpenKeyExW"
     #-doors.unicode "RegOpenKeyExA"
                   open-reg-key*)
    (:stdcall advapi32)
  (long rv (if (zerop rv)
             result
             (error 'windows-error :code (hresult-from-win32 rv))))
  "Opens the specified registry key. Note that key names are not case sensitive."
  (key handle)
  (subkey (& tstring :in t) :optional)
  (reserved dword :aux 0)
  (desired-access registry-rights)
  (result (& handle :out) :aux))

(define-external-function
    (#+doors.unicode "RegQueryValueExW"
     #-doors.unicode "RegQueryValueExA"
                   query-reg-value*)
    (:stdcall advapi32)
  (long rv (if (zerop rv)
             (values buffer-size type)
             (error 'windows-error :code (hresult-from-win32 rv))))
  "Retrieves the type and data for the specified value name associated with an open registry key."
  (key handle)
  (value (& tstring :in t) :key)
  (reserved pointer :aux &0)
  (type (& registry-value-type :out) :aux)
  (data-buffer pointer)
  (buffer-size (& dword :inout)))

#-(or win2000 winxp winserver2003 winhomeserver)
(define-external-function
    (#+doors.unicode "RegGetValueW"
     #-doors.unicode "RegGetValueA"
                   reg-value)
    (:stdcall advapi32)
  (long rv (if (zerop rv)
             (values buffer-size type)
             (error 'windows-error :code (hresult-from-win32 rv))))
  "Retrieves the type and data for the specified registry value."
  (key handle)
  (subkey (& tstring :in t) :key)
  (value (& tstring :in t) :key)
  (flags registry-type-restrictions :key)
  (type (& registry-value-type :out) :aux)
  (data-buffer pointer)
  (buffer-size (& dword :inout)))
