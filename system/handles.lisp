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
    ("CloseHandle" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool))
  "Closes an open object handle."
  (handle handle))

(define-external-function
    ("DuplicateHandle" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv target-handle)
  "Duplicates an object handle."
  (process handle :key (external-function-call
                         "GetCurrentProcess"
                         ((:stdcall kernel32)
                          (handle))))
  (source-handle handle)
  (target-process handle :key process)
  (target-handle (& handle :out) :aux)
  (desired-access dword :key)
  (inherit-handle boolean :key)
  (options (enum (:base-type dword :list t)
             (:close-source 1)
             (:same-access 2))
           :key :same-access))

(define-enum (handle-flags
               (:list t)
               (:conc-name handle-flag-))
  (:inherit 1)
  (:protect-from-close 2))

(define-external-function
    ("GetHandleInformation" handle-information)
    (:stdcall kernel32)
  ((last-error bool) rv flags)
  "Retrieves certain properties of an object handle."
  (object handle)
  (flags (& handle-flags :out) :aux))

(define-external-function
    ("SetHandleInformation" (setf handle-information))
    (:stdcall kernel32)
  ((last-error bool) rv flags)
  "Sets certain properties of an object handle."
  (object handle :optional)
  (mask handle-flags :aux flags)
  (flags handle-flags))
