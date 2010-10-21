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

(in-package #:doors.com)

(defvar *registered-com-objects* (make-array 0 :adjustable t :fill-pointer 0))

(defclass com-object ()
  ((ref-count :initform 0
              :accessor com-object-ref-count)
   (interface-pointers :initform (make-hash-table :test #'eq)
                       :reader com-object-interface-pointers)))

(defmethod shared-initialize :after ((object com-object) slot-names
                                      &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (unless (find object *registered-com-objects* :test #'eq)
    (finalize object (let ((pointers (com-object-interface-pointers object)))
                       (lambda ()
                         (maphash (lambda (k v)
                                    (declare (ignore k)) (free v 'pointer))
                           pointers)))))
  object)

(defgeneric query-interface (object name))
(defgeneric add-ref (object))
(defgeneric release (object))

(defun acquire-interface (object class &optional finalize)
  (declare (type com-object object)
           (type (or symbol com-interface-class) class))
  (unless (typep class 'com-interface-class)
    (setf class (find-interface-class class)))
  (let* ((pointer (or (gethash class (com-object-interface-pointers object))
                      (let* ((vtable (symbol-value
                                       (com-interface-class-vtable-name class)))
                             (pointer (alloc '* vtable)))
                        (setf (gethash (&& pointer) *pointer-to-object-mapping*)
                              object
                              (gethash class (com-object-interface-pointers object))
                              pointer)
                        pointer)))
         (interface (translate-interface pointer class finalize)))
    (add-ref object)
    interface))

(defmethod query-interface ((object com-object) (name uuid))
  (values nil
          name
          (acquire-interface object (find-interface-class-by-iid name))))

(defmethod add-ref ((object com-object))
  (or (position object *registered-com-objects* :test #'eq)
      (vector-push-extend object *registered-com-objects*))
  (incf (com-object-ref-count object)))

(defmethod release ((object com-object))
  (let ((ref-count (com-object-ref-count object)))
    (if (> ref-count 0)
      (progn
        (when (= ref-count 1)
          (let* ((registry-length (length *registered-com-objects*))
                 (pos (position object *registered-com-objects* :test #'eq)))
            (when (> pos (1+ registry-length))
              (replace *registered-com-objects*
                       *registered-com-objects*
                       :start1 pos :start2 (1+ pos)))
            (adjust-array *registered-com-objects*
                          (1- registry-length)
                          :fill-pointer t)))
        (decf (com-object-ref-count object)))
      0)))
