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

(declaim (inline rect make-rect copy-rect rect-p
                 rect-left rect-top rect-right rect-bottom
                 (setf rect-left) (setf rect-top)
                 (setf rect-right) (setf rect-bottom)))
(define-struct (rect
                 (:constructor make-rect)
                 (:constructor rect (left top right bottom)))
    "The RECT structure defines the coordinates of the upper-left and lower-right corners of a rectangle."
  (left long)
  (top long)
  (right long)
  (bottom long))

(declaim (inline point make-point copy-point point-p
                 pt-x pt-y (setf pt-x) (setf pt-y)))
(defstruct (point
             (:conc-name pt-)
             (:constructor make-point)
             (:constructor point (x y)))
    "The POINT structure defines the x- and y- coordinates of a point."
  (x 0 :type long)
  (y 0 :type long))

(define-immediate-type point-type ()
  ()
  (:base-type qword)
  (:lisp-type (type) 'point)
  (:simple-parser point)
  (:prototype (type) (point 0 0))
  (:prototype-expansion (type) `(point 0 0))
  (:translator (value type)
    (point (make-long (ldb (byte 16 0) value)
                      (ldb (byte 16 16) value))
           (make-long (ldb (byte 16 32) value)
                      (ldb (byte 16 48) value))))
  (:translator-expansion (value type)
    (once-only ((value `(the qword ,value)))
      `(point (make-long (ldb (byte 16 0) ,value)
                         (ldb (byte 16 16) ,value))
              (make-long (ldb (byte 16 32) ,value)
                         (ldb (byte 16 48) ,value)))))
  (:converter (point type)
    (make-qword (pt-x point) (pt-y point)))
  (:converter-expansion (point type)
    (once-only ((point `(the point ,point)))
      `(make-qword (pt-x ,point) (pt-y ,point))))
  (:allocator-expansion (value type)
    `(alloc 'qword))
  (:deallocator-expansion (pointer type)
    `(free ,pointer))
  (:cleaner-expansion (pointer value type)
    ()))

(declaim (inline point* make-point* copy-point* point-p*
                 pt-x* pt-y* (setf pt-x*) (setf pt-y*)))
(defstruct (point*
             (:conc-name pt-)
             (:constructor make-point*
                           (&key (x 0) (y 0) &aux (x* x) (y* y)))
             (:constructor point* (x y &aux (x* x) (y* y))))
    "The POINT* structure defines the x- and y- coordinates of a point."
  (x* 0 :type short)
  (y* 0 :type short))

(define-immediate-type point-type* ()
  ()
  (:base-type dword)
  (:lisp-type (type) 'point*)
  (:simple-parser point*)
  (:prototype (type) (point* 0 0))
  (:prototype-expansion (type) `(point* 0 0))
  (:translator (value type)
    (point* (make-short (ldb (byte 8 0) value)
                        (ldb (byte 8 8) value))
            (make-short (ldb (byte 8 16) value)
                        (ldb (byte 8 24) value))))
  (:translator-expansion (value type)
    (once-only ((value `(the dword ,value)))
      `(point* (make-short (ldb (byte 8 0) ,value)
                           (ldb (byte 8 8) ,value))
               (make-short (ldb (byte 8 16) ,value)
                           (ldb (byte 8 24) ,value)))))
  (:converter (point type)
    (make-dword (pt-x* point) (pt-y* point)))
  (:converter-expansion (point type)
    (once-only ((point `(the point* ,point)))
      `(make-dword (pt-x* ,point) (pt-y* ,point))))
  (:allocator-expansion (value type)
    `(alloc 'dword))
  (:deallocator-expansion (pointer type)
    `(free ,pointer))
  (:cleaner-expansion (pointer value type)
    ()))
