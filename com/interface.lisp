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

(defvar *pointer-to-object-mapping* (make-weak-hash-table :test #'eql
                                      :weakness :value))

(defvar *pointer-to-interface-mapping* (make-weak-hash-table :test #'equal
                                         :weakness :value))

(defvar *iid-to-interface-class-mapping* (make-hash-table :test #'equalp))

(eval-when (:compile-toplevel :load-toplevel :execute)

(closer-mop:defclass com-interface-class (standard-class)
  ((%iid :initform nil :initarg :iid)   
   (vtable-name :initform nil
                :initarg :vtable-name
                :reader com-interface-class-vtable-name)
   (methods :initform '()
            :initarg :methods
            :reader com-interface-class-methods)))

(closer-mop:defmethod closer-mop:validate-superclass
    ((class standard-class) (superclass com-interface-class))
  nil)

(closer-mop:defmethod closer-mop:validate-superclass
    ((class com-interface-class) (superclass standard-class))
  t)
  
(closer-mop:defclass com-interface ()
  ((com-pointer :initform &0 :initarg :pointer)
   (finalize :initarg :finalize :allocation :class
             :initform nil))
  (:metaclass com-interface-class))
  
(closer-mop:defclass com-generic-function (closer-mop:standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))
  
(closer-mop:defmethod no-applicable-method
    ((function com-generic-function) &rest args)
  (declare (ignore args))
  (error 'com-error :code error-not-implemented))
  
(closer-mop:finalize-inheritance (find-class 'com-interface-class))
(closer-mop:finalize-inheritance (find-class 'com-interface))
(closer-mop:finalize-inheritance (find-class 'com-generic-function))
  
) ;;eval-when

(deftype iid () '(or symbol com-interface-class guid))

(defun find-interface-class (name &optional (errorp T))
  (declare (type (or symbol guid) name))
  (let ((class (etypecase name
                 (symbol (find-class name nil))
                 (guid (gethash name *iid-to-interface-class-mapping*)))))
    (if (typep class 'com-interface-class)
      class
      (and errorp (error 'com-error :code error-no-interface)))))

(defconstant pointer-slot-location
    (closer-mop:slot-definition-location
        (find 'com-pointer
              (closer-mop:class-slots (find-class 'com-interface))
              :key #'closer-mop:slot-definition-name)))

(declaim (inline com-interface-pointer))
(defun com-interface-pointer (interface)
  (declare (type com-interface interface))
  (closer-mop:standard-instance-access interface pointer-slot-location))

(declaim (inline com-interface-method-pointer))
(defun com-interface-method-pointer (interface method-name)
  (declare (type com-interface interface)
           (type symbol method-name))
  (let* ((class (class-of interface))
         (index (or (position method-name
                              (com-interface-class-methods class)
                              :test #'eq)
                    (error "Interface ~s has no method named ~s"
                           (class-name class) method-name))))
    (deref (deref (com-interface-pointer interface) 'pointer)
           'pointer
           (* index (sizeof 'pointer)))))

(defmethod uuid-of ((class com-interface-class))
  (slot-value class '%iid))

(defun translate-interface (pointer class &optional finalize)
  (declare (type pointer pointer)
           (type (or symbol guid com-interface-class) class)
           (optimize (speed 3)))
  (unless (typep class 'com-interface-class)
    (setf class (find-interface-class class)))
  (if (&? pointer)
    (let* ((address (the size-t (&& pointer)))
           (typed-pointer (cons address class))
           (interface (gethash typed-pointer *pointer-to-interface-mapping*)))
      (if interface
        (when finalize
          (initialize-instance interface :finalize t))
        (setf (gethash typed-pointer *pointer-to-interface-mapping*)
              (setf interface (make-instance class
                                :pointer pointer
                                :finalize finalize))))
      interface)
    nil))

(declaim (inline convert-interface))
(defun convert-interface (interface)
  (declare (type (or null com-interface) interface))
  (if (null interface)
    &0
    (com-interface-pointer interface)))

(define-immediate-type com-interface-type ()
  ((name :initform nil
         :initarg :name
         :reader com-interface-type-name)
   (finalize :initform nil
             :initarg :finalize
             :reader com-interface-type-finalize-p))
  (:base-type pointer)
  (:lisp-type (type) `(or null ,(com-interface-type-name type)))
  (:prototype (type) nil)
  (:prototype-expansion (type) nil)
  (:converter (lisp-value type)    
    (convert-interface lisp-value))
  (:translator (pointer type)
    (translate-interface pointer
                         (find-interface-class
                           (com-interface-type-name type))
                         (com-interface-type-finalize-p type)))
  (:converter-expansion (lisp-value-form type)
    `(convert-interface ,lisp-value-form))
  (:translator-expansion (pointer-form type)
    `(translate-interface ,pointer-form
                          (find-interface-class
                            ',(com-interface-type-name type))
                          ,(and (com-interface-type-finalize-p type)
                                T)))
  (:allocator-expansion (value type)
    `(alloc 'pointer))
  (:deallocator-expansion (pointer type)
    `(free ,pointer '*))
  (:cleaner-expansion (pointer value type)
    nil))

(defmethod unparse-type ((type com-interface-type))
  (com-interface-type-name type))

(define-translatable-type iid-type ()
  ()
  (:simple-parser iid)
  (:lisp-type (type) 'iid)
  (:prototype (type) (guid 0 0 0 0 0 0 0 0 0 0 0))
  (:prototype-expansion (type) '(guid 0 0 0 0 0 0 0 0 0 0 0))
  (:fixed-size (type) (sizeof 'guid))
  (:cleaner (value pointer type) nil)
  (:cleaner-expansion (value pointer type) nil)
  (:allocator (value type) (alloc 'guid))
  (:allocator-expansion (value type) `(alloc 'guid))
  (:deallocator (pointer type) (free pointer 'guid))
  (:deallocator-expansion (pointer type) `(free ,pointer 'guid))
  (:reader (pointer out type)
    (let ((guid (or (and (typep out 'guid) out)
                    (prototype type))))
      (or (find-interface-class (deref pointer 'guid 0 guid) nil)
          guid)))
  (:reader-expansion (pointer out type)
    (once-only ((pointer `(the pointer ,pointer))
                (out `(the iid ,out)))
      (with-gensyms (guid)
        `(let ((,guid (or (and (guidp ,out) ,out)
                          ,(expand-prototype type))))
           (declare (type guid ,guid))
           (or (find-interface-class (deref ,pointer 'guid 0 ,guid) nil)
               ,guid)))))
  (:writer (value pointer type)
    (let* ((class (if (typep value 'com-interface-class)
                    value
                    (find-interface-class value)))
           (guid (or (slot-value class '%iid)
                     (error 'com-error :code error-no-interface))))
      (setf (deref pointer 'guid) guid)
      value))
  (:writer-expansion (value pointer type)
    (once-only (value (pointer `(the pointer ,pointer)))
      (with-gensyms (class guid)
        `(let* ((,class (the com-interface-class
                             (if (typep ,value 'com-interface-class)
                               ,value
                               (find-interface-class ,value))))
                (,guid (or (slot-value ,class '%iid)
                           (error 'com-error :code error-no-interface))))
           (setf (deref ,pointer 'guid) (the guid ,guid))
           ,value)))))
