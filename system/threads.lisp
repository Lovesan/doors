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

(define-external-function
    ("AttachThreadInput" (:camel-case))
    (:stdcall user32)
  (bool rv (if rv
             attach
             #-(or win2000 winxp winxp64 winserver2003 winhomeserver)
             (error 'windows-error :code (hresult-from-win32 last-error))
             #+(or win2000 winxp winxp64 winserver2003 winhomeserver)
             (error 'windows-error :code error-invalid-arg)))
  "Attaches or detaches the input processing mechanism of one thread to that of another thread."
  (id-attach dword)
  (id-attach-to dword)
  (attach boolean))

(define-external-function
    ("GetCurrentThreadId" current-thread-id)
    (:stdcall kernel32)
  (dword)
  "Retrieves the thread identifier of the calling thread.")

(define-symbol-macro current-thread-id (current-thread-id))
