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

(defun %error-no-interface (value)
  (declare (ignore value))
  (error 'windows-error :code error-no-interface))
(defun %iid-known-p (iid)
  (declare (type iid iid))
  (not (null (find-interface-class-by-iid iid nil))))

(defalias known-iid ()
  `(filtered iid %iid-known-p %error-no-interface))

(define-guid iid-unknown
  #x00000000 #x0000 #x0000
  #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46)

(define-interface unknown (iid-unknown)
  "Lisp wrapper for IUnknown interface"
  (query-interface
    (hresult rv
      (translate-interface
        (com-interface-pointer interface)
        (find-interface-class-by-iid iid)
        T))
    (iid (& known-iid))
    (interface (& unknown :out) :aux))
  (add-ref (ulong))
  (release (ulong)))

(defmethod shared-initialize :after
  ((object unknown) slot-names &rest initargs &key finalize &allow-other-keys)
  (declare (ignore slot-names initargs))
  (when finalize
    (let ((pobject (com-interface-pointer object)))
      (declare (type pointer pobject))
      (when (&? pobject)
        (finalize object (lambda ()
                           (external-pointer-call
                             (deref (&+ (deref pobject '*) 2 '*) '*)
                             ((:stdcall)
                              (ulong)
                              (pointer this :aux pobject))))))))
  object)

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
