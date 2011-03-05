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

(define-interface unknown
    ((iid-unknown "{00000000-0000-0000-C000-000000000046}"))
  "Lisp wrapper for IUnknown interface"
  (query-interface
    ((hresult com-error) rv
      (translate-interface
        (com-interface-pointer interface) iid T))
    (iid (& iid))
    (interface (& unknown :out) :aux))
  (add-ref (ulong))
  (release (ulong)))

(defmethod add-ref :around ((object unknown))
  (symbol-macrolet ((ref-count (aref (the (simple-array fixnum (1))
                                          (closer-mop:standard-instance-access
                                            object
                                            ref-count-slot-location))
                                     0)))
      (prog1 (call-next-method)
       (incf ref-count))))

(defmethod release :around ((object unknown))
  (symbol-macrolet ((ref-count (aref (the (simple-array fixnum (1))
                                          (closer-mop:standard-instance-access
                                            object
                                            ref-count-slot-location))
                                     0)))
    (if (> ref-count 0)
      (prog1 (call-next-method)
       (decf ref-count))
      0)))

(defmethod shared-initialize :after
  ((object unknown) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let* ((pobject (com-interface-pointer object))
         (thread (bt:current-thread))
         (thread-id (%current-tid))
         (apartment (%apartment-type))
         (ref-count (closer-mop:standard-instance-access object ref-count-slot-location))
         (finalizer (lambda (unknown count)
                      (declare (type pointer unknown)
                               (type non-negative-fixnum count))
                      (without-interrupts
                        (dotimes (i count)
                          (%release unknown))))))
    (declare (type pointer pobject)
             (type dword thread-id)
             (type function finalizer)
             (type (simple-array fixnum (1)) ref-count))
    (when (&? pobject)
      (finalize object (lambda ()
                         #+thread-support
                         (let ((this-thread-id (%current-tid))
                               (this-apartment (%apartment-type)))
                           (if (= thread-id this-thread-id)
                             (when (eq apartment this-apartment)
                               (funcall finalizer pobject (aref ref-count 0)))
                             (case apartment
                               (:sta (when (bt:thread-alive-p thread)
                                       (bt:interrupt-thread thread
                                         (lambda (finalizer apartment unknown count)
                                           (when (eq apartment (%apartment-type))
                                             (funcall finalizer unknown count)))
                                         finalizer apartment pobject (aref ref-count 0))))
                               (T (bt:with-lock-held (*mta-post-mortem-lock*)
                                    (push (list finalizer pobject (aref ref-count 0))
                                          *mta-post-mortem-queue*)
                                    (bt:condition-notify *mta-post-mortem-condvar*))))))
                         #-thread-support
                         (when (eq apartment (%apartment-type))
                           (funcall finalizer pobject (aref ref-count 0))))))))

(defmacro with-interface ((var interface) &body body)
  `(let ((,var ,interface))
     (unwind-protect
         (locally ,@body)
       (release ,var))))

(defmacro with-interfaces ((&rest specs) &body body)
  (if (null specs)
    `(locally ,@body)
    `(with-interface ,(car specs)
       (with-interfaces ,(rest specs)
         ,@body))))
