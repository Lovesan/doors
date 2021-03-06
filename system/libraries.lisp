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

(define-foreign-library ntdll
  (T (:default "ntdll")))

(define-foreign-library kernel32
  (T (:default "kernel32")))

(define-foreign-library user32
  (T (:default "user32")))

(define-foreign-library comctl32
  (T (:default "comctl32")))

(define-foreign-library gdi32
  (T (:default "gdi32")))

(define-foreign-library ws2-32
  (T (:default "ws2_32")))

(define-foreign-library advapi32
  (T (:default "advapi32")))

(define-foreign-library psapi
  (t (:default "psapi")))

(define-foreign-library ole32
  (t (:default "ole32")))

(define-foreign-library oleaut32
  (t (:default "oleaut32")))

(define-foreign-library secur32
  (t (:default "secur32")))

(use-foreign-library ntdll)
(use-foreign-library kernel32)
(use-foreign-library user32)
(use-foreign-library comctl32)
(use-foreign-library gdi32)
(use-foreign-library ws2-32)
(use-foreign-library advapi32)
(use-foreign-library psapi)
(use-foreign-library ole32)
(use-foreign-library oleaut32)
(use-foreign-library secur32)
