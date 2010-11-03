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

(declaim (inline guid guid-dw guid-w1 guid-w2
                 guid-b1 guid-b2 guid-b3 guid-b4
                 guid-b5 guid-b6 guid-b7 guid-b8
                 %guid-reader %guid-writer %guid-cleaner))
(define-struct
    (guid
      (:constructor guid (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8))
      (:cleaner %guid-cleaner)
      (:reader %guid-reader)
      (:writer %guid-writer)
      (:print-object (lambda (object stream)
                       (print-unreadable-object (object stream :type t)
                         (with-accessors
                             ((dw guid-dw) (w1 guid-w1) (w2 guid-w2)
                              (b1 guid-b1) (b2 guid-b2) (b3 guid-b3) (b4 guid-b4)
                              (b5 guid-b5) (b6 guid-b6) (b7 guid-b7) (b8 guid-b8))
                             object
                           (format
                             stream
                             "{~8,'0X-~{~4,'0X-~}~{~2,'0X~}-~{~2,'0X~}}"
                             dw
                             (list w1 w2)
                             (list b1 b2)
                             (list b3 b4 b5 b6 b7 b8))))
                       object)))
  (dw uint32)
  (w1 uint16)
  (w2 uint16)
  (b1 uint8)
  (b2 uint8)
  (b3 uint8)
  (b4 uint8)
  (b5 uint8)
  (b6 uint8)
  (b7 uint8)
  (b8 uint8))

(defmacro with-guid-accessors ((dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
                               guid
                               &body body)
  `(with-accessors ((,dw guid-dw) (,w1 guid-w1) (,w2 guid-w2)
                    (,b1 guid-b1) (,b2 guid-b2) (,b3 guid-b3) (,b4 guid-b4)
                    (,b5 guid-b5) (,b6 guid-b6) (,b7 guid-b7) (,b8 guid-b8))
       (the guid ,guid)
     ,@body))

(defun %guid-reader (p o)
  (declare (type pointer p))
  (let ((out (or o (guid 0 0 0 0 0 0 0 0 0 0 0))))
    (declare (type guid out))
    (with-guid-accessors
        (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8) out
      (setf dw (deref p 'dword (offsetof 'guid 'dw))
            w1 (deref p 'word (offsetof 'guid 'w1))
            w2 (deref p 'word (offsetof 'guid 'w2))
            b1 (deref p 'byte (offsetof 'guid 'b1))
            b2 (deref p 'byte (offsetof 'guid 'b2))
            b3 (deref p 'byte (offsetof 'guid 'b3))
            b4 (deref p 'byte (offsetof 'guid 'b4))
            b5 (deref p 'byte (offsetof 'guid 'b5))
            b6 (deref p 'byte (offsetof 'guid 'b6))
            b7 (deref p 'byte (offsetof 'guid 'b7))
            b8 (deref p 'byte (offsetof 'guid 'b8)))
      out)))
(defun %guid-writer (v p)
  (declare (type pointer p) (type guid v))
  (with-guid-accessors
      (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8) v
    (setf (deref p 'dword (offsetof 'guid 'dw)) dw
          (deref p 'word (offsetof 'guid 'w1)) w1
          (deref p 'word (offsetof 'guid 'w2)) w2
          (deref p 'byte (offsetof 'guid 'b1)) b1
          (deref p 'byte (offsetof 'guid 'b2)) b2
          (deref p 'byte (offsetof 'guid 'b3)) b3
          (deref p 'byte (offsetof 'guid 'b4)) b4
          (deref p 'byte (offsetof 'guid 'b5)) b5
          (deref p 'byte (offsetof 'guid 'b6)) b6
          (deref p 'byte (offsetof 'guid 'b7)) b7
          (deref p 'byte (offsetof 'guid 'b8)) b8)
    v))
(defun %guid-cleaner (p v)
  (declare (ignore p v))
  nil)

(declaim (inline guid-equal))
(defun guid-equal (guid1 guid2)
  (declare (type guid guid1 guid2))
  (with-accessors
      ((dw-1 guid-dw) (w1-1 guid-w1) (w2-1 guid-w2)
       (b1-1 guid-b1) (b2-1 guid-b2) (b3-1 guid-b3) (b4-1 guid-b4)
       (b5-1 guid-b5) (b6-1 guid-b6) (b7-1 guid-b7) (b8-1 guid-b8))
      guid1
    (with-accessors
        ((dw-2 guid-dw) (w1-2 guid-w1) (w2-2 guid-w2)
         (b1-2 guid-b1) (b2-2 guid-b2) (b3-2 guid-b3) (b4-2 guid-b4)
         (b5-2 guid-b5) (b6-2 guid-b6) (b7-2 guid-b7) (b8-2 guid-b8))
        guid2
      (and (= dw-1 dw-2) (= w1-1 w1-2) (= w2-1 w2-2)
           (= b1-1 b1-2) (= b2-1 b2-2) (= b3-1 b3-2) (= b4-1 b4-2)
           (= b5-1 b5-2) (= b6-1 b6-2) (= b7-1 b7-2) (= b8-1 b8-2)))))

(defalias uuid () 'guid)
(deftype uuid () 'guid)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *guid-constants* (make-hash-table :test #'equalp))
  (defmethod make-load-form ((object guid) &optional env)
    (declare (ignore env))
    (or (gethash object *guid-constants*)
        (with-guid-accessors (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
          object
          `(guid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8)))))

(defmacro define-guid (name dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
  (check-type name symbol)
  (check-type dw dword)
  (check-type w1 word)
  (check-type w2 word)
  (check-type b1 ubyte)
  (check-type b2 ubyte)
  (check-type b3 ubyte)
  (check-type b4 ubyte)
  (check-type b5 ubyte)
  (check-type b6 ubyte)
  (check-type b7 ubyte)
  (check-type b8 ubyte)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-constant ,name (guid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8)
       :test #'equalp)
     (setf (gethash ,name *guid-constants*) ',name)
     ',name))

(define-guid uuid-null  0 0 0 0 0 0 0 0 0 0 0)

(defgeneric uuid-of (class)
  (:method (class)
    (error 'windows-error :code error-invalid-arg))
  (:method ((class symbol))
    (uuid-of (find-class class)))
  (:method ((class null))
    uuid-null))

(define-compiler-macro uuid-of (&whole form class)
  (if (constantp class)
    (uuid-of (eval class))
    form))
