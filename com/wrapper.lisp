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

(closer-mop:defmethod shared-initialize :around ((class com-wrapper-class) slot-names &rest initargs
                                                 &key direct-superclasses &allow-other-keys)
  (remf initargs :direct-superclasses)
  (let ((direct-superclasses (if (find-if (lambda (superclass)
                                            (subtypep (class-name superclass) 'com-wrapper))
                                          direct-superclasses)
                               direct-superclasses
                               (append direct-superclasses (list (find-class 'com-wrapper))))))
    (apply #'call-next-method
           class
           slot-names
           :direct-superclasses direct-superclasses
           initargs)))

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
                                                 (mapcar (constantly (find-class t))
                                                   (cdr primary)))
                                          (list* class (mapcar (constantly (find-class t))
                                                         primary)))
                                        :function function))))
                                  (%interface-class-wrapper-functions
                                    interface-class))
                              interface-class)
                      (remove-duplicates (cons (find-class 'unknown) interfaces)))))
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
         (unknown-class (find-class 'unknown))
         (unknown (with-pointer (pmqi (make-multi-qi :iid unknown-class)
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
         (unknown-cookie (%add-to-git unknown))
         (interfaces (setf (%wrapper-interface-pointers object)
                           (make-hash-table :test #'eq)))
         (interface-cookies (make-hash-table :test #'eq))
         (token (%com-wrapper-token object))
         (finalizer (lambda (token)
                      (declare (type cons token))
                      (destructuring-bind
                          (unknown-cookie . interface-cookies) token
                        (when unknown-cookie
                          (%revoke-from-git unknown-cookie)
                          (setf (car token) nil))
                        (when interface-cookies
                          (loop :for cookie :of-type dword :being :the :hash-values
                            :of interface-cookies
                            :do (%revoke-from-git cookie))
                          (setf (cdr token) nil))))))
    (declare (type pointer unknown)
             (type cons token)
             (type dword unknown-cookie)
             (type function finalizer)
             (type hash-table interfaces interface-cookies))
    (setf (car token) unknown-cookie
          (cdr token) interface-cookies)
    (finalize object (lambda ()
                       #+thread-support
                       (bt:with-lock-held (*mta-post-mortem-lock*)
                         (push (list finalizer token)
                               *mta-post-mortem-queue*)
                         (bt:condition-notify *mta-post-mortem-condvar*))
                       #-thread-support
                       (funcall finalizer token)))
    (setf (gethash 'unknown interfaces) unknown)
    (dolist (interface-class (remove unknown-class (slot-value class '%interfaces)))
      (let ((interface (external-pointer-call
                        (deref (deref unknown '*) '*)
                        ((:stdcall)
                         (hresult rv ptr)
                         (pointer this :aux unknown)
                         ((& iid) iid :aux interface-class)
                         ((& pointer :out) ptr :aux &0)))))
        (setf (gethash (class-name interface-class) interfaces)
              interface
              (gethash (class-name interface-class) interface-cookies)
              (%add-to-git interface))))))

(closer-mop:defmethod add-ref :around ((object com-wrapper))
  (if (car (the cons (%com-wrapper-token object)))
    1
    0))

(closer-mop:defmethod release :around ((object com-wrapper))
  (let ((token (the cons (%com-wrapper-token object))))
    (destructuring-bind
        (unknown-cookie . interface-cookies) token
      (when unknown-cookie
        (%revoke-from-git unknown-cookie)
        (setf (car token) nil))
      (when interface-cookies
        (loop :for cookie :of-type dword :being :the :hash-values
          :of interface-cookies
          :do (%revoke-from-git cookie))
        (setf (cdr token) nil))))
  0)
