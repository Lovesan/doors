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

(defvar *clsid-to-com-class-mapping* (make-hash-table :test #'equalp))
(defvar *registered-com-objects* (make-array 0 :adjustable t :fill-pointer 0))

(deftype clsid () '(or symbol guid com-class))

(closer-mop:defclass com-object ()
  ((ref-count :initform 0 :accessor com-object-ref-count)
   (interface-pointers :initform (make-hash-table :test #'eq)
                       :reader com-object-interface-pointers))
  (:documentation
    "All lisp-side COM object classes must inherit from this class."))
  
(closer-mop:defclass com-class (com-object standard-class)
  ((%clsid :initform nil :initarg :clsid)))
  
(closer-mop:defmethod shared-initialize :after
  ((class com-class) slot-names &rest initargs &key clsid &allow-other-keys)
  (declare (ignore slot-names initargs))
  (unless (null clsid)    
    (setf (gethash (setf (slot-value class '%clsid)
                         (etypecase clsid
                           (guid clsid)
                           (string (external-function-call
                                     "CLSIDFromString"
                                     ((:stdcall ole32)
                                      (hresult rv id)
                                      ((& wstring) str :aux clsid)
                                      ((& guid :out) id :aux))))
                           (symbol (let ((id (symbol-value clsid)))
                                     (check-type id clsid)
                                     id))
                           (cons (apply #'guid clsid))))
                   *clsid-to-com-class-mapping*)
          class)))
  
(closer-mop:defmethod closer-mop:validate-superclass
    ((class com-class) (superclass standard-class))
  T)
  
(closer-mop:defmethod closer-mop:validate-superclass
    ((class standard-class) (superclass com-class))
  T)

(defmethod uuid-of ((class com-class))
  (slot-value class '%clsid))

(defun find-com-class (name &optional (errorp t))
  (declare (type (or symbol guid) name))
  (let ((class (etypecase name
                 (symbol (find-class name nil))
                 (guid (gethash name *clsid-to-com-class-mapping*)))))
    (if (typep class 'com-class)
      class
      (and errorp (error 'com-error :code error-class-not-registered)))))

(defmethod shared-initialize :after ((object com-object) slot-names
                                      &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (unless (find object *registered-com-objects* :test #'eq)
    (finalize object (let ((pointers (com-object-interface-pointers object)))
                       (lambda ()
                         (maphash (lambda (k v)
                                    (declare (ignore k)) (free v 'pointer))
                           pointers))))))

(defgeneric query-interface (object name)
  (:documentation
    "Retrieves pointers to the supported interfaces on an object.")
  (:generic-function-class com-generic-function))
(defgeneric add-ref (object)
  (:documentation
    "Increments the reference count for an interface on an object.")
  (:generic-function-class com-generic-function))
(defgeneric release (object)
  (:documentation
    "Decrements the reference count for an interface on an object.")
  (:generic-function-class com-generic-function))

(defmethod no-applicable-method ((f (eql #'query-interface)) &rest args)
  (declare (ignore args))
  (error 'com-error :code error-not-implemented))

(defun acquire-interface (object class &optional finalize)
  (declare (type com-object object)
           (type iid class))
  "Acquires specified interface wrapper for an object."
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
    (when (and finalize (not (subtypep (class-name class)
                                       'unknown)))
      (finalize interface (lambda () (release object))))
    (add-ref object)
    interface))

(defmethod query-interface ((object com-object) class)
  (values nil class (acquire-interface object class)))

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
            (when (< pos (1- registry-length))
              (replace *registered-com-objects*
                       *registered-com-objects*
                       :start1 pos :start2 (1+ pos)))
            (adjust-array *registered-com-objects*
                          (1- registry-length)
                          :fill-pointer t)))
        (decf (com-object-ref-count object)))
      0)))

(define-translatable-type clsid-type ()
  ()
  (:simple-parser clsid)
  (:lisp-type (type) 'clsid)
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
      (or (find-com-class (deref pointer 'guid 0 guid) nil)
          guid)))
  (:reader-expansion (pointer out type)
    (once-only ((pointer `(the pointer ,pointer))
                (out `(the clsid ,out)))
      (with-gensyms (guid)
        `(let ((,guid (or (and (guidp ,out) ,out)
                          ,(expand-prototype type))))
           (declare (type guid ,guid))
           (or (find-com-class (deref ,pointer 'guid 0 ,guid) nil)
               ,guid)))))
  (:writer (value pointer type)
    (declare (type clsid value))
    (let ((guid (if (typep value 'guid)
                  value
                  (or (slot-value
                        (if (typep value 'com-class)
                          value
                          (find-com-class value))
                        '%clsid)
                      (error 'com-error :code error-class-not-registered)))))
      (setf (deref pointer 'guid) guid)
      value))
  (:writer-expansion (value pointer type)
    (once-only ((value `(the clsid ,value))
                (pointer `(the pointer ,pointer)))
      (with-gensyms (guid)
        `(let ((,guid (if (typep ,value 'guid)
                        ,value
                        (or (slot-value
                              (if (typep ,value 'com-class)
                                ,value
                                (find-com-class ,value))
                              '%clsid)
                            (error 'com-error :code error-class-not-registered)))))
           (setf (deref ,pointer 'guid) (the guid ,guid))
           ,value)))))

(defmethod convert-value (lisp-value (type com-interface-type))
  (etypecase lisp-value
    (null &0)
    (com-interface (com-interface-pointer lisp-value))
    (com-wrapper (or (gethash (com-interface-type-name type)
                              (%wrapper-interface-pointers lisp-value))
                     (error 'com-error :code error-no-interface)))
    (com-object  (prog1
                  (com-interface-pointer
                    (funcall 'acquire-interface
                             lisp-value
                             (com-interface-type-name type)))
                  (funcall 'release lisp-value)))))
