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

(in-package #:doors.com)

(define-external-function
    ("CoRegisterClassObject" register-class-object)
    (:stdcall ole32)
  (hresult rv register)
  "Registers an EXE class object with OLE so other applications can connect to it."
  (clsid (& clsid))
  (unknown unknown :aux (if (typep clsid 'com-class)
                          clsid
                          (find-com-class clsid)))
  (class-context class-context-flags :optional :server)
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
    ("CoInitialize" initialize-com)
    (:stdcall ole32)
  (dword rv (handler-bind
              ((windows-status (lambda (c)
                                 (external-function-call
                                   "CoUninitialize"
                                   ((:stdcall ole32)
                                    (void)))
                                 (muffle-warning c))))
              (translate rv 'hresult)))
  "Initializes the COM library on the current thread and identifies the concurrency model as single-thread apartment (STA)."
  (reserved pointer :aux &0))

(define-external-function
    ("CoUninitialize" uninitialize-com)
    (:stdcall ole32)
  (void rv (values))
  "Closes the COM library on the current thread, unloads all DLLs loaded by the thread, frees any other resources that the thread maintains, and forces all RPC connections on the thread to close. ")

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

(define-external-function
    ("StringFromIID" string-from-iid)
    (:stdcall ole32)
  (hresult rv (prog1
               (translate lpsz '(& wstring))
               (task-mem-free lpsz)))
  "Converts an interface identifier into a string of printable characters."
  (iid (& iid))
  (lpsz (& pointer :out) :aux))

(define-external-function
    ("IIDFromString" iid-from-string)
    (:stdcall ole32)
  (hresult rv iid)
  "Converts a string generated by the STRING-FROM-IID function back into the original interface identifier (IID)."
  (string (& wstring))
  (iid (& iid :out) :aux))

(define-external-function
    ("CoInitializeEx" initialize-com*)
    (:stdcall ole32)
  (dword rv (handler-bind
              ((windows-status (lambda (c)
                                 (external-function-call
                                   "CoUninitialize"
                                   ((:stdcall ole32)
                                    (void)))
                                 (muffle-warning c))))
              (translate rv 'hresult)))
  "Initializes the COM library for use by the calling thread, sets the thread's concurrency model, and creates a new apartment for the thread if one is required."
  (reserved pointer :aux &0)
  (flags com-init-flags :optional))

(define-external-function
    ("CoGetObject" com-object)
    (:stdcall ole32)
  (hresult rv (translate-interface
                (com-interface-pointer object)
                iid
                t))
  "Converts a display name into a moniker that identifies the object named, and then binds to the object identified by the moniker."
  (name (& wstring))
  (bind-options (& bind-options :in t) :optional)
  (iid (& iid))
  (object (& unknown :out) :aux))

(define-external-function
    ("CLSIDFromProgID" clsid-from-progid)
    (:stdcall ole32)
  (hresult rv clsid)
  "Looks up a CLSID in the registry, given a ProgID."
  (progid (& wstring))
  (clsid (& clsid :out) :aux))

(define-external-function
    ("CLSIDFromProgIDEx" clsid-from-progid*)
    (:stdcall ole32)
  (hresult rv clsid)
  "Triggers automatic installation if the COMClassStore policy is enabled."
  (progid (& wstring))
  (clsid (& clsid :out) :aux))

(define-external-function
    ("CLSIDFromString" clsid-from-string)
    (:stdcall ole32)
  (hresult rv clsid)
  "Converts a string generated by the string-from-clsid function back into the original CLSID. "
  (progid (& wstring))
  (clsid (& clsid :out) :aux))

(define-external-function
    ("StringFromCLSID" string-from-clsid)
    (:stdcall ole32)
  (hresult rv (prog1
               (translate lpsz '(& wstring))
               (task-mem-free lpsz)))
  "Converts a CLSID into a string of printable characters."
  (clsid (& clsid))
  (lpsz (& pointer :out) :aux))

(define-external-function
    ("ProgIDFromCLSID" progid-from-clsid)
    (:stdcall ole32)
  (hresult rv (prog1
               (translate lpsz '(& wstring))
               (task-mem-free lpsz)))
  "Retrieves the ProgID for a given CLSID. "
  (clsid (& clsid))
  (lpsz (& pointer :out) :aux))

(define-external-function
    ("StringFromGUID2" string-from-guid)
    (:stdcall ole32)
  ((last-error int doors::not-zero) rv buffer)
  "Converts a globally unique identifier (GUID) into a string of printable characters. "
  (guid (& guid))
  (buffer (& (~ wchar nil simple-string) :out)
          :aux (make-string 38 :initial-element #\space))
  (max int :aux 39))

(define-external-function
    ("CoCreateInstanceEx" create-com-instance*)
    (:stdcall ole32)
  (hresult rv results)
  "Creates an instance of a specific class on a specific computer."
  (clsid (& clsid))
  (outer unknown :key)
  (context class-context-flags :key :server)
  (server-info (& server-info :in t) :key)
  (count dword :key (array-total-size results))
  (results (& (simple-array multi-qi) :inout)))

(define-external-function
    ("CoAddRefServerProcess" add-ref-server-process)
    (:stdcall ole32)
  (ulong)
  "Increments a global per-process reference count")

(define-external-function
    ("CoReleaseServerProcess" release-server-process)
    (:stdcall ole32)
  (ulong)
  "Decrements the global per-process reference count.")

(define-external-function
    ("CoResumeClassObjects" resume-class-objects)
    (:stdcall ole32)
  (hresult)
  "Called by a server that can register multiple class objects to inform the SCM about all registered classes, and permits activation requests for those class objects.")

(define-external-function
    ("CoSuspendClassObjects" suspend-class-objects)
    (:stdcall ole32)
  (hresult)
  "Prevents any new activation requests from the SCM on all class objects registered within the process.")
