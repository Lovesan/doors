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

(in-package #:doors.com)

(closer-mop:defclass com-wrapper-class (com-class)
  ())

(closer-mop:defmethod closer-mop:validate-superclass
    ((class com-wrapper-class) (superclass com-wrapper-class))
  nil)

(closer-mop:defmethod shared-initialize :after
  ((class com-wrapper-class) slot-names &rest initargs
   &key interfaces &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let ((interfaces (mapcar (lambda (interface-class)
                              (unless (typep interface-class
                                             'com-interface-class)
                                (setf interface-class
                                      (find-interface-class interface-class)))
                              (assert (guidp (uuid-of interface-class))
                                  (interface-class)
                                "Interface class' IID is invalid")
                              (maphash
                                  (lambda (name fspec &aux (gf (fdefinition name)))
                                    (destructuring-bind
                                        (function . primary) fspec
                                    (add-method
                                      gf
                                      (make-instance 'closer-mop:standard-method
                                        :lambda-list
                                        (closer-mop:generic-function-lambda-list gf)
                                        :specializers
                                        (if (consp name)
                                          (list* (find-class t)
                                                 class
                                                 (mapcar (constantly
                                                           (find-class t))
                                                   (cddr primary)))
                                          (list* class (mapcar (constantly
                                                                 (find-class t))
                                                         (cdr primary))))
                                        :function function))))
                                  (%interface-class-wrapper-functions
                                    interface-class))
                              interface-class)
                      interfaces)))
    (setf (slot-value class '%interfaces) interfaces)))

(closer-mop:defmethod shared-initialize :after
  ((object com-wrapper) slot-names &rest initargs
   &key context server-info &allow-other-keys)
  (declare (ignore slot-names initargs))
  (check-type server-info (or null void server-info))
  (unless server-info (setf server-info void))
  (let* ((context (convert context 'class-context-flags))
         (class (let ((class (class-of object)))
                  (assert (typep class 'com-wrapper-class) ()
                    'type-error :datum class :expected-type 'com-wrapper-class)
                  class))
         (unknown (with-pointer (pmqi (make-multi-qi :iid 'unknown)
                                      'multi-qi)
                    (external-function-call
                      "CoCreateInstanceEx"
                      ((:stdcall ole32)
                       (hresult rv)
                       ((& clsid) clsid :aux class)
                       (pointer aggregate :aux &0)
                       (dword ctx :aux context)
                       ((& server-info :in t) sinfo :aux server-info)
                       (dword count :aux 1)
                       (pointer results :aux pmqi)))
                    (deref pmqi 'hresult (offsetof 'multi-qi 'hresult))
                    (deref pmqi 'pointer (offsetof 'multi-qi 'interface))))
         (interfaces (make-hash-table :test #'eq))
         (thread (bt:current-thread))
         (thread-id (%current-tid))
         (apartment (%apartment-type))
         (finalizer (lambda (unknown interfaces)
                      (declare (type pointer unknown)
                               (type hash-table interfaces))
                      (without-interrupts
                        (%release unknown)
                        (maphash (lambda (class pointer)
                                   (declare (ignore class))
                                   (%release pointer))
                          interfaces)))))
    (declare (type pointer unknown)
             (type function finalizer))
    (finalize object (lambda ()
                       #+thread-support
                       (let ((this-thread-id (%current-tid))
                             (this-apartment (%apartment-type)))
                         (if (= this-thread-id thread-id)
                           (when (eq apartment this-apartment)
                             (funcall finalizer unknown interfaces))
                           (case apartment
                             (:sta (when (bt:thread-alive-p thread)
                                     (bt:interrupt-thread thread
                                       (lambda (finalizer apartment unknown interfaces)
                                         (when (eq apartment (%apartment-type))
                                           (funcall finalizer unknown interfaces)))
                                       finalizer apartment unknown interfaces)))
                             (T (bt:with-lock-held (*mta-post-mortem-lock*)
                                  (push (list finalizer unknown interfaces)
                                        *mta-post-mortem-queue*)
                                  (bt:condition-notify *mta-post-mortem-condvar*))))))
                       #-thread-support
                       (when (eq apartment (%apartment-type))
                         (funcall finalizer unknown interfaces))))
    (dolist (interface-class (slot-value class '%interfaces))
      (setf (gethash (class-name interface-class) interfaces)
            (prog1 (external-pointer-call
                     (deref (deref unknown '*) '*)
                     ((:stdcall)
                      (hresult rv ptr)
                      (pointer this :aux unknown)
                      ((& iid) iid :aux interface-class)
                      ((& pointer :out) ptr :aux &0))))))
    (setf (%wrapper-interface-pointers object) interfaces)))
