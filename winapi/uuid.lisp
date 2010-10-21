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

(declaim (inline uuid uuid-dw uuid-w1 uuid-w2
                 uuid-b1 uuid-b2 uuid-b3 uuid-b4
                 uuid-b5 uuid-b6 uuid-b7 uuid-b8))
(define-struct
    (uuid
      (:constructor uuid (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8))
      (:print-object (lambda (object stream)
                       (print-unreadable-object (object stream :type t)
                         (with-accessors
                             ((dw uuid-dw) (w1 uuid-w1) (w2 uuid-w2)
                              (b1 uuid-b1) (b2 uuid-b2) (b3 uuid-b3) (b4 uuid-b4)
                              (b5 uuid-b5) (b6 uuid-b6) (b7 uuid-b7) (b8 uuid-b8))
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

(defmacro with-uuid-accessors ((dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
                               uuid
                               &body body)
  `(with-accessors ((,dw uuid-dw) (,w1 uuid-w1) (,w2 uuid-w2)
                    (,b1 uuid-b1) (,b2 uuid-b2) (,b3 uuid-b3) (,b4 uuid-b4)
                    (,b5 uuid-b5) (,b6 uuid-b6) (,b7 uuid-b7) (,b8 uuid-b8))
       (the uuid ,uuid)
     ,@body))

(declaim (inline uuid-equal))
(defun uuid-equal (uuid1 uuid2)
  (declare (type uuid uuid1 uuid2))
  (with-accessors
      ((dw-1 uuid-dw) (w1-1 uuid-w1) (w2-1 uuid-w2)
       (b1-1 uuid-b1) (b2-1 uuid-b2) (b3-1 uuid-b3) (b4-1 uuid-b4)
       (b5-1 uuid-b5) (b6-1 uuid-b6) (b7-1 uuid-b7) (b8-1 uuid-b8))
      uuid1
    (with-accessors
        ((dw-2 uuid-dw) (w1-2 uuid-w1) (w2-2 uuid-w2)
         (b1-2 uuid-b1) (b2-2 uuid-b2) (b3-2 uuid-b3) (b4-2 uuid-b4)
         (b5-2 uuid-b5) (b6-2 uuid-b6) (b7-2 uuid-b7) (b8-2 uuid-b8))
        uuid2
      (and (= dw-1 dw-2) (= w1-1 w1-2) (= w2-1 w2-2)
           (= b1-1 b1-2) (= b2-1 b2-2) (= b3-1 b3-2) (= b4-1 b4-2)
           (= b5-1 b5-2) (= b6-1 b6-2) (= b7-1 b7-2) (= b8-1 b8-2)))))

(declaim (inline guid    guid-dw guid-w1 guid-w2
                 guid-b1 guid-b2 guid-b3 guid-b4
                 guid-b5 guid-b6 guid-b7 guid-b8))
(define-struct (guid (:include uuid)
                     (:constructor guid (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8))))

(defmacro with-guid-accessors ((dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
                               guid
                               &body body)
  `(with-accessors ((,dw guid-dw) (,w1 guid-w1) (,w2 guid-w2)
                    (,b1 guid-b1) (,b2 guid-b2) (,b3 guid-b3) (,b4 guid-b4)
                    (,b5 guid-b5) (,b6 guid-b6) (,b7 guid-b7) (,b8 guid-b8))
       (the guid ,guid)
     ,@body))

(declaim (inline guid-equal))
(defun guid-equal (guid1 guid2)
  (declare (type guid guid1 guid2))
  (uuid-equal guid1 guid2))

(declaim (inline iid    iid-dw iid-w1 iid-w2
                 iid-b1 iid-b2 iid-b3 iid-b4
                 iid-b5 iid-b6 iid-b7 iid-b8))
(define-struct (iid (:include guid)
                    (:constructor iid (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8))))

(defmacro with-iid-accessors ((dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
                               iid
                               &body body)
  `(with-accessors ((,dw iid-dw) (,w1 iid-w1) (,w2 iid-w2)
                    (,b1 iid-b1) (,b2 iid-b2) (,b3 iid-b3) (,b4 iid-b4)
                    (,b5 iid-b5) (,b6 iid-b6) (,b7 iid-b7) (,b8 iid-b8))
       (the iid ,iid)
     ,@body))

(declaim (inline iid-equal))
(defun iid-equal (iid1 iid2)
  (declare (type iid iid1 iid2))
  (guid-equal iid1 iid2))

(declaim (inline clsid    clsid-dw clsid-w1 clsid-w2
                 clsid-b1 clsid-b2 clsid-b3 clsid-b4
                 clsid-b5 clsid-b6 clsid-b7 clsid-b8))
(define-struct (clsid (:include guid)
                      (:constructor clsid (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8))))

(defmacro with-clsid-accessors ((dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
                                clsid
                                &body body)
  `(with-accessors ((,dw clsid-dw) (,w1 clsid-w1) (,w2 clsid-w2)
                    (,b1 clsid-b1) (,b2 clsid-b2) (,b3 clsid-b3) (,b4 clsid-b4)
                    (,b5 clsid-b5) (,b6 clsid-b6) (,b7 clsid-b7) (,b8 clsid-b8))
       (the clsid ,clsid)
     ,@body))

(declaim (inline clsid-equal))
(defun clsid-equal (clsid1 clsid2)
  (declare (type clsid clsid1 clsid2))
  (guid-equal clsid1 clsid2))

(declaim (inline fmtid    fmtid-dw fmtid-w1 fmtid-w2
                 fmtid-b1 fmtid-b2 fmtid-b3 fmtid-b4
                 fmtid-b5 fmtid-b6 fmtid-b7 fmtid-b8))
(define-struct (fmtid (:include guid)
                      (:constructor fmtid (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8))))

(defmacro with-fmtid-accessors ((dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
                               fmtid
                               &body body)
  `(with-accessors ((,dw fmtid-dw) (,w1 fmtid-w1) (,w2 fmtid-w2)
                    (,b1 fmtid-b1) (,b2 fmtid-b2) (,b3 fmtid-b3) (,b4 fmtid-b4)
                    (,b5 fmtid-b5) (,b6 fmtid-b6) (,b7 fmtid-b7) (,b8 fmtid-b8))
       (the fmtid ,fmtid)
     ,@body))

(declaim (inline fmtid-equal))
(defun fmtid-equal (fmtid1 fmtid2)
  (declare (type fmtid fmtid1 fmtid2))
  (guid-equal fmtid1 fmtid2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *uuid-constants* (make-hash-table :test #'equalp))
  (defvar *guid-constants* (make-hash-table :test #'equalp))
  (defvar *iid-constants* (make-hash-table :test #'equalp))
  (defvar *clsid-constants* (make-hash-table :test #'equalp))
  (defvar *fmtid-constants* (make-hash-table :test #'equalp)))

(defmethod make-load-form ((object uuid) &optional env)
  (declare (ignore env))
  (or (gethash object *uuid-constants*)
      (with-uuid-accessors (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
        object
        `(uuid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8))))

(defmethod make-load-form ((object guid) &optional env)
  (declare (ignore env))
  (or (gethash object *guid-constants*)
      (with-guid-accessors (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
        object
        `(guid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8))))

(defmethod make-load-form ((object iid) &optional env)
  (declare (ignore env))
  (or (gethash object *iid-constants*)
      (with-iid-accessors (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
        object
        `(iid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8))))

(defmethod make-load-form ((object clsid) &optional env)
  (declare (ignore env))
  (or (gethash object *clsid-constants*)
      (with-clsid-accessors (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
        object
        `(clsid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8))))

(defmethod make-load-form ((object fmtid) &optional env)
  (declare (ignore env))
  (or (gethash object *fmtid-constants*)
      (with-fmtid-accessors (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
        object
        `(fmtid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8))))

(defmacro define-uuid (name dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
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
     (define-constant ,name (uuid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8)
       :test #'equalp)
     (setf (gethash ,name *uuid-constants*) ',name)
     ',name))

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

(defmacro define-iid (name dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
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
     (define-constant ,name (iid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8)
       :test #'equalp)
     (setf (gethash ,name *iid-constants*) ',name)
     ',name))

(defmacro define-clsid (name dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
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
     (define-constant ,name (clsid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8)
       :test #'equalp)
     (setf (gethash ,name *clsid-constants*) ',name)
     ',name))

(defmacro define-fmtid (name dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)
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
     (define-constant ,name (fmtid ,dw ,w1 ,w2 ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8)
       :test #'equalp)
     (setf (gethash ,name *fmtid-constants*) ',name)
     ',name))

(defmacro define-ole-uuid (name dw w1 w2)
  `(define-uuid ,name ,dw ,w1 ,w2 #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))
(defmacro define-ole-guid (name dw w1 w2)
  `(define-guid ,name ,dw ,w1 ,w2 #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))
(defmacro define-ole-iid (name dw w1 w2)
  `(define-iid ,name ,dw ,w1 ,w2 #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))
(defmacro define-ole-clsid (name dw w1 w2)
  `(define-clsid ,name ,dw ,w1 ,w2 #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))
(defmacro define-ole-fmtid (name dw w1 w2)
  `(define-fmtid ,name ,dw ,w1 ,w2 #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))

(define-uuid  uuid-null  0 0 0 0 0 0 0 0 0 0 0)
(define-guid  guid-null  0 0 0 0 0 0 0 0 0 0 0)
(define-iid   iid-null   0 0 0 0 0 0 0 0 0 0 0)
(define-clsid clsid-null 0 0 0 0 0 0 0 0 0 0 0)
(define-fmtid fmtid-null 0 0 0 0 0 0 0 0 0 0 0)

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

(define-struct (objectid
                 (:constructor objectid (lineage uniquifier)))
  (lineage    guid)
  (uniquifier dword))
