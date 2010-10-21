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

(define-external-function ("GetLastError" (:camel-case))
    (:stdcall kernel32)
  (dword))

(define-external-function ("SetLastError" (:camel-case))
    (:stdcall kernel32)
  (void)
  (error-code dword))

(defun last-error (&optional (error-if-no-error T) default-value)
  (let ((last-error (get-last-error)))
    (if (system-error-code-p last-error)
      (let ((result (hresult-from-win32 last-error)))
        (if (hresult-error-p result)
          (error 'windows-error :code result)
          (if error-if-no-error
            (error 'windows-error :code error-failure)
            default-value)))
      (error 'non-system-error :code last-error))))

(defun %last-error (value)
  (declare (ignore value))
  (last-error))

(declaim (inline not-null))
(defun not-null (x)
  (not (null x)))

(declaim (inline not-zero))
(defun not-zero (x)
  (not (zerop x)))

(defalias last-error (type &optional (predicate 'not-null))
  `(filtered ,type ,predicate %last-error))
