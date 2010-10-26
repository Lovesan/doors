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

(define-struct (security-attributes
                 (:conc-name security-)
                 (:constructor security-attributes
                               (descriptor &optional inherit-handle)))
  (attributes-struct-size
    dword :initform (sizeof 'security-attributes))
  (descriptor pointer)
  (inherit-handle bool))

(define-struct (luid (:constructor luid (low-part high-part)))
  (low-part dword)
  (high-part long))

(define-enum (trustee-form
               (:conc-name trustee-is-))
  :sid
  :name
  :bad-form
  :objects-and-sid
  :objects-and-name)

(define-enum (trustee-type
               (:conc-name trustee-is-))
  :unknown
  :user
  :group
  :domain
  :alias
  :well-known-group
  :deleted
  :invalid
  :computer)

(define-enum (se-object-type (:conc-name se-))
  :unknown-object-type
  :file-object
  :service
  :printer
  :registry-key
  :lmshare
  :kernel-object
  :window-object
  :ds-object
  :ds-object-all
  :provider-defined-object
  :wmi-guid-object
  :registry-wow64-32-key)

(define-struct (objects-and-name)
  (objects-present (enum (:base-type dword)
                         (:object-type-present 1)
                         (:inherited-object-type-present 2)))
  (object-type se-object-type)
  (object-type-name (& tstring))
  (inherited-object-type-name (& tstring))
  (name (& tstring)))

(define-struct (objects-and-sid)
  (objects-present (enum (:base-type dword)
                         (:object-type-present 1)
                         (:inherited-object-type-present 2)))
  (object-type-guid guid)
  (inherited-object-type-guid guid)
  (sid pointer))

(define-struct (trustee
                 (:reader %trustee-reader)
                 (:constructor trustee (form type name)))
  (reserved pointer :initform &0)
  (multiple-trustee-operation
    (enum () :no-multiple-trustee-operation)
    :initform :no-multiple-trustee-operation)
  (form trustee-form)
  (type trustee-type)
  (name (union ()
          (sid pointer)
          (name (& tstring))
          (objects-and-name (& objects-and-name))
          (objects-and-sid (& objects-and-sid)))))

(declaim (inline %trustee-reader))
(defun %trustee-reader (p out)
  (declare (type pointer p))
  (let* ((form (deref p 'trustee-form (offsetof 'trustee 'form)))
         (out (or out (trustee
                        form
                        (deref p 'trustee-type (offsetof 'trustee 'type))
                        &0))))
    (setf (trustee-name out)
          (case form
            (:name (deref p 'tstring (offsetof 'trustee 'name)))
            (:objects-and-name (deref p 'objects-and-name
                                      (offsetof 'trustee 'name)))
            (:objects-and-sid (deref p 'objects-and-sid
                                     (offsetof 'trustee 'name)))
            (T (deref p 'pointer (offsetof 'trustee 'name)))))
    out))
