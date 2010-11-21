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

(in-package #:doors.com)

(define-external-function
    ("CoRegisterClassObject" register-class-object)
    (:stdcall ole32)
  (hresult rv register)
  "Registers an EXE class object with OLE so other applications can connect to it."
  (clsid (& clsid))
  (unknown unknown)
  (class-context class-context-flags :optional :all)
  (flags class-object-registration-flags :optional :multiple-use)
  (register (& dword :out) :aux))

(define-external-function
    ("CoRevokeClassObject" revoke-class-object)
    (:stdcall ole32)
  (hresult)
  "Informs OLE that a class object is no longer available for use."
  (register-token dword))

(define-external-function
    ("CoGetClassObject" class-object)
    (:stdcall ole32)
  (hresult rv (translate-interface
                (com-interface-pointer object)
                iid
                T))
  "Provides a pointer to an interface on a class object associated with a specified CLSID. "
  (clsid (& clsid))
  (context class-context-flags :optional :server)
  (server-info (& server-info :in t) :optional void)
  (iid (& iid))
  (object (& unknown :out) :aux))

(define-external-function
    ("CoCreateInstance" create-com-instance)
    (:stdcall ole32)
  (hresult rv (translate-interface
                (com-interface-pointer object)
                iid
                t))
  "Creates an instance of a specific class on a specific computer."
  (clsid (& clsid))
  (aggregate-unknown unknown :optional)
  (context class-context-flags :optional :server)
  (iid (& iid))
  (object (& unknown :out) :aux))

(define-external-function
    ("CoInitialize" initialize)
    (:stdcall ole32)
  (hresult)
  "Initializes the COM library on the current thread and identifies the concurrency model as single-thread apartment (STA)."
  (reserved pointer :aux &0))

(define-external-function
    ("CoUninitialize" uninitialize)
    (:stdcall ole32)
  (void)
  "Closes the COM library on the current thread, unloads all DLLs loaded by the thread, frees any other resources that the thread maintains, and forces all RPC connections on the thread to close. ")

(define-external-function
    ("CoCreateGuid" create-guid)
    (:stdcall ole32)
  (hresult rv guid)
  "Creates a GUID, a unique 128-bit integer used for CLSIDs and interface identifiers. "
  (guid (& guid :out) :aux))

(define-external-function
    ("CoTaskMemAlloc" task-mem-alloc)
    (:stdcall ole32)
  (pointer rv (if (or (&? rv) (zerop size))
                rv
                (error 'windows-error :code error-out-of-memory)))
  "Allocates a block of task memory."
  (size size-t))

(define-external-function
    ("CoTaskMemFree" task-mem-free)
    (:stdcall ole32)
  (void)
  "Frees a block of task memory."
  (pointer pointer))

(define-external-function
    ("CoTaskMemRealloc" task-mem-realloc)
    (:stdcall ole32)
  (pointer rv (if (or (&? rv) (zerop size))
                rv
                (error 'windows-error :code error-out-of-memory)))
  "Changes the size of a previously allocated block of task memory."
  (pointer pointer :optional &0)
  (size size-t))
