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
      (translate-interface interface iid T))
    (iid (& iid))
    (interface (& pointer :out) :aux))
  (add-ref (ulong))
  (release (ulong)))

(closer-mop:defmethod add-ref :around ((object unknown))
  (let ((token (%com-interface-token object)))
    (unless (cdr token)
      (call-next-method)
      (setf (cdr token) (%add-to-git (com-interface-pointer object))))
    1))

(closer-mop:defmethod release :around ((object unknown))
  (let ((token (%com-interface-token object)))
    (when (cdr token)
      (%revoke-from-git (cdr token))
      (setf (cdr token) nil))
    0))

(closer-mop:defmethod shared-initialize :after
  ((object unknown) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let* ((token (%com-interface-token object))
         (finalizer (lambda (token)
                      (declare (type cons token))
                      (when (cdr token)
                        (%revoke-from-git (cdr token))
                        (setf (cdr token) nil)))))
    (declare (type function finalizer)
             (type cons token))
    (setf (car token) t)
    (finalize object (lambda ()
                       #+thread-support
                       (bt:with-lock-held (*mta-post-mortem-lock*)
                         (push (list finalizer token) *mta-post-mortem-queue*)
                         (bt:condition-notify *mta-post-mortem-condvar*))
                       #-thread-support
                       (funcall finalizer token)))))

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
