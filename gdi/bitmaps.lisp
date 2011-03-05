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

(in-package #:doors.gdi)

(declaim (inline size make-size copy-size size-p
                 size-cx size-cy
                 (setf size-cx) (setf size-cy)))
(define-struct (size
                 (:constructor make-size)
                 (:constructor size (cx cy)))
    "The SIZE structure specifies the width and height of a rectangle."
  (cx long)
  (cy long))

(define-struct (blend-function
                 (:conc-name bf-)
                 (:constructor make-bf (&optional (source-constant-alpha 255))))
    "The BLEND-FUNCTION structure controls blending by specifying the blending functions for source and destination bitmaps."
  (blend-op byte :initform 0)
  (blend-flags byte :initform 0)
  (source-constant-alpha byte)
  (alpha-format byte :initform 1))
