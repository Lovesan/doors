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

(define-external-function
    ("DisableThreadLibraryCalls" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool))
  (module handle))

(define-external-function
    ("FreeLibrary" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool))
  "Frees the loaded dynamic-link library (DLL) module and, if necessary, decrements its reference count."
  (module handle))

(define-external-function
    ("FreeLibraryAndExitThread" (:camel-case))
    (:stdcall kernel32)
  (void)
  "Decrements the reference count of a loaded dynamic-link library (DLL) by one, then calls ExitThread to terminate the calling thread. The function does not return."
  (module handle)
  (exit-code dword :optional 0))

#-win2000
(define-external-function
    (#+doors.unicode "GetDllDirectoryW"
     #-doors.unicode "GetDllDirectoryA"
                   dll-directory)
    (:stdcall kernel32)
  (dword rv (if (zerop rv)
              (invoke-last-error)
              (subseq buffer 0 rv)))
  "Retrieves the application-specific portion of the search path used to locate DLLs for the application."
  (buffer-length dword :optional 256)
  (buffer (& tstring :out) :aux (make-string buffer-length)))

#-win2000
(define-symbol-macro dll-directory (dll-directory))

(define-external-function
    (#+doors.unicode "GetModuleFileNameW"
     #-doors.unicode "GetModuleFileNameA"
                   module-file-name)
    (:stdcall kernel32)
  (dword rv (if (and (zerop rv) (not (zerop buffer-size)))
              (invoke-last-error)
              (subseq buffer 0 rv)))
  "Retrieves the fully-qualified path for the file that contains the specified module."
  (module handle :optional nil)
  (buffer (& tstring :out) :aux (make-string buffer-size))
  (buffer-size dword :optional 256))

(define-symbol-macro module-file-name (module-file-name))

(define-external-function
    (#+doors.unicode "GetModuleHandleW"
     #-doors.unicode "GetModuleHandleA"
                   module-handle)
    (:stdcall kernel32)
  ((last-error handle))
  "Retrieves a module handle for the specified module. The module must have been loaded by the calling process."
  (module-name (& tstring :in t) :optional void))

(define-symbol-macro module-handle (module-handle))

(define-enum (module-handle-ex-flags
               (:base-type dword)
               (:list t)
               (:conc-name module-handle-ex-flag-))
  (:inc-refcount #x00000000)
  (:from-address #x00000004)
  (:pin #x00000001)
  (:unchanged-refcount #x00000002))

#-win2000
(define-external-function
    (#+doors.unicode "GetModuleHandleExW"
     #-doors.unicode "GetModuleHandleExA"
                   module-handle-ex)
    (:stdcall kernel32)
  ((last-error bool) rv module)
  "Retrieves a module handle for the specified module. The module must have been loaded by the calling process."
  (flags module-handle-ex-flags)
  (module-name (union ()
                      pointer
                      (& tstring :in t))
               :optional void)
  (module (& handle :out) :aux))

#-win2000
(define-symbol-macro module-handle-ex (module-handle-ex))

(define-external-function
    ("GetProcAddress" proc-address)
    (:stdcall kernel32)
  ((last-error pointer &?))
  "Retrieves the address of an exported function or variable from the specified dynamic-link library (DLL)."
  (module handle)
  (proc-name (union ()
                    (number size-t)
                    (name (& astring)))))

(define-external-function
    (#+doors.unicode "LoadLibraryW"
     #-doors.unicode "LoadLibraryA"
                   load-library)
    (:stdcall kernel32)
  ((last-error handle))
  "Loads the specified module into the address space of the calling process."
  (filename (& tstring)))

(define-enum (load-library-ex-flags
               (:base-type dword)
               (:list t)
               (:conc-name load-library-))
  (:dont-resolve-dll-references #x00000001)
  (:ignore-code-authz-level #x00000010)
  (:as-datafile #x00000002)
  (:as-datafile-exclusive #x00000040)
  (:as-image-resource #x00000020)
  (:with-altered-search-path #x00000008))

(define-external-function
    (#+doors.unicode "LoadLibraryExW"
     #-doors.unicode "LoadLibraryExA"
                   load-library-ex)
    (:stdcall kernel32)
  ((last-error handle))
  "Loads the specified module into the address space of the calling process. The specified module may cause other modules to be loaded."
  (filename (& tstring))
  (hfile handle :aux nil)
  (flags load-library-ex-flags))

(define-struct (load-params
                (:constructor make-load-params)
                (:constructor
                  load-params (cmd-line &optional cmd-show env-address)))
  (env-address (& (~ (& astring)) :in t) :initform void)
  (cmd-line (& pascal-string))
  (cmd-show (& dword) :initform #x00000002)
  (reserved dword :initform 0))

(define-external-function
    ("LoadModule" (:camel-case))
    (:stdcall kernel32)
  (dword rv (if (> rv 31)
              t
              (error 'windows-error
                     :code (if (zerop rv)
                             error-out-of-memory
                             (hresult-from-win32 rv)))))
  "Loads and executes an application or creates a new instance of an existing application."
  (module-name (& astring))
  (parameter-block (& load-params)))

#-win2000
(define-external-function
    (#+doors.unicode "SetDllDirectoryW"
     #-doors.unicode "SetDllDirectoryA"
                   (setf dll-directory))
    (:stdcall kernel32)
  ((last-error bool) rv pathname)
  "Adds a directory to the search path used to locate DLLs for the application."
  (pathname (& tstring) :optional void))
