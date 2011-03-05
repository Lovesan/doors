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

(in-package #:doors.ui)

(define-enum (keystroke-flag
               (:base-type dword)
               (:conc-name kf-))
  (:extended  #x0100)
  (:dlg-mode  #x0800)
  (:menu-mode #x1000)
  (:alt-down  #x2000)
  (:repeat    #x4000)
  (:up        #x8000))

(declaim (inline key-repeat-count))
(defun key-repeat-count (lparam)
  (declare (type lparam lparam))
  (ldb (byte 16 0) lparam))

(declaim (inline key-scan-code))
(defun key-scan-code (lparam)
  (declare (type lparam lparam))
  (ldb (byte 8 16) lparam))

(declaim (inline key-flags))
(defun key-flags (lparam)
  (declare (type lparam lparam))
  (translate (ldb (byte 16 16) lparam) 'keystroke-flag))

(define-enum (hit-test-code
               (:base-type uint)
               (:conc-name ht))
  (:foo 1))

