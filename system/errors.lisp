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

(define-results windows-status (warning)
  ()
  ((ok 0 "No error occurred")
   (false 1 "Successful but nonstandard completion of operation"))
  (:conc-name status-)
  (:default-initargs :code status-false))

(defun windows-status-code (windows-status)
  (declare (type windows-status windows-status))
  (slot-value windows-status 'code))

(define-results windows-error (error)
  ()
  ((success 0 "No error occurred")
   (unexpected-failure #x8000FFFF
     "Catastrophic failure")
   (not-implemented #x80004001
     "Not implemented")
   (out-of-memory #x8007000E
     "Ran out of memory")
   (invalid-arg #x80070057
     "One or more arguments are invalid")
   (no-interface #x80004002
     "No such interface supported")
   (invalid-pointer #x80004003
     "Invalid pointer")
   (invalid-handle #x80070006
     "Invalid handle")
   (insufficient-buffer #x8007007A
     "The data area passed to a system call is too small")
   (abort #x80004004
     "Operation aborted")
   (failure #x80004005
     "Unspecified error")
   (access-denied #x80070005
     "General access denied error")
   (data-pending #x8000000A
     "The data necessary to complete this operation is not yet available"))
  (:conc-name error-)
  (:default-initargs :code error-failure))

(defun windows-error-code (windows-error)
  (declare (type windows-error windows-error))
  (slot-value windows-error 'code))

(define-condition non-system-error (windows-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Non-system error. Code: ~s"
                     (windows-condition-code condition))
             condition)))

(declaim (inline system-error-code-p))
(defun system-error-code-p (code)
  (declare (type dword code))
  (not (logbitp 29 code)))

(define-external-function ("GetLastError" last-error)
    (:stdcall kernel32)
  (dword))

(define-external-function ("SetLastError" (setf last-error))
    (:stdcall kernel32)
  (void rv error-code)
  (error-code dword))

(define-symbol-macro last-error (last-error))

(defun invoke-last-error (&optional (error-if-no-error T) default-value)
  (let ((last-error-code last-error))
    (if (system-error-code-p last-error-code)
      (let ((result (hresult-from-win32 last-error-code)))
        (if (hresult-error-p result)
          (error 'windows-error :code result)
          (if error-if-no-error
            (error 'windows-error :code error-failure)
            default-value)))
      (error 'non-system-error :code last-error-code))))

(defun %invoke-last-error (value)
  (declare (ignore value))
  (invoke-last-error))

(declaim (inline not-null))
(defun not-null (x)
  (not (null x)))

(declaim (inline not-zero))
(defun not-zero (x)
  (not (zerop x)))

(defalias last-error (type &optional (predicate 'not-null))
  `(filtered ,type ,predicate %invoke-last-error))

(define-external-function "Beep"
    (:stdcall kernel32)
  ((last-error boolean))
  (frequency dword)
  (duration dword))

#-win2000
(define-external-function
    ("CaptureStackBackTrace" (:camel-case))
    (:stdcall kernel32)
  (ushort rv (values (subseq backtrace 0 rv)
                     back-trace-hash))
  (frames-to-skil ulong)
  (frames-to-capture ulong)
  (backtrace (& (simple-array pointer) :out)
             :aux (make-array frames-to-capture))
  (back-trace-hash (& ulong :out) :aux))

(define-external-function
    (#+doors.unicode "FatalAppExitW"
     #-doors.unicode "FatalAppExitA"
                   fatal-app-exit)
    (:stdcall kernel32)
  (void)
  (action uint :aux 0)
  (message-text (& tstring)))

(define-external-function
    ("FlashWindow" (:camel-case))
    (:stdcall user32)
  (boolean)
  (hwnd handle)
  (invert boolean))

(define-enum (flash-window-flags
               (:conc-name flashw-))
  (:stop 0)
  (:caption 1)
  (:tray 2)
  (:all 3)
  (:timer 4)
  (:timer-no-fg #xC))

(define-struct (flash-window-info
                 (:conc-name flash-window-)
                 (:constructor flash-window-info
                               (&key hwnd flags count timeout)))
  (size uint :initform (sizeof 'flash-window-info))
  (hwnd handle)
  (flags flash-window-flags)
  (count uint)
  (timeout dword))

(define-external-function
    ("FlashWindowEx" (:camel-case))
    (:stdcall user32)
  (boolean)
  (fwinfo (& flash-window-info)))

(define-enum (format-message-flags
               (:base-type dword)
               (:conc-name format-message-))
  (:allocate-buffer #x00000100)
  (:argument-array  #x00002000)
  (:from-module     #x00000800)
  (:from-string     #x00000400)
  (:from-system     #x00001000)
  (:ignore-inserts  #x00000200)  
  (:max-width-mask  #x000000FF))

(define-external-function
    (#+doors.unicode "FormatMessageW"
     #-doors.unicode "FormatMessageA"
                   format-message)
    (:stdcall kernel32)
  ((last-error dword not-zero))
  (flags format-message-flags)
  (source pointer :key)
  (message-id dword :key)
  (language-id dword :key 0)
  (buffer pointer)
  (size dword :key)
  (arguments pointer :key))

(define-enum (system-error-mode
               (:conc-name sem-)
               (:base-type uint))
  (:fail-critical-errors 1)
  (:no-alignment-fault-exception 4)
  (:no-page-fault-error-box 2)
  (:no-open-file-error-box #x8000))

#-(or :win2000 :winxp :winx64 :winserver2003 :winhomeserver)
(define-external-function
    ("GetErrorMode" error-mode)
    (:stdcall kernel32)
  (system-error-mode))

#-(or :win2000 :winxp :winx64 :winserver2003 :winhomeserver :winvista
      :winserver2008)
(define-external-function
    ("GetThreadErrorMode" thread-error-mode)
    (:stdcall kernel32)
  (system-error-mode))

#-(or :win2000 :winxp :winx64 :winserver2003 :winhomeserver)
(define-external-function
    ("SetErrorMode" (setf error-mode))
    (:stdcall kernel32)
  (system-error-mode)
  (system-error-mode))

#-(or :win2000 :winxp :winx64 :winserver2003 :winhomeserver
      :winvista :winserver2008)
(define-external-function
    ("SetThreadErrorMode" (setf thread-error-mode))
    (:stdcall kernel32)
  (system-error-mode)
  (system-error-mode))

(define-symbol-macro error-mode (error-mode))
(define-symbol-macro thread-error-mode (thread-error-mode))

(define-external-function
    ("MessageBeep" (:camel-case))
    (:stdcall user32)
  ((last-error boolean))
  (type (enum (:base-type uint)
              (:simple #xFFFFFFFF)
              (:asterisk #x40)
              (:exclamation #x30)
              (:error #x10)
              (:hand #x10)
              (:information #x40)
              (:question #x20)
              (:stop #x10)
              (:warning #x30)
              (:ok 0))
        :optional :simple))
