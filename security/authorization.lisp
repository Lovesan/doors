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

(in-package #:doors.security)

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

(defalias trustee-name () '(union ()
                            (sid pointer)
                            (name (& tstring))
                            (objects-and-name (& objects-and-name))
                            (objects-and-sid (& objects-and-sid))))

(define-struct (trustee
                 (:reader %trustee-reader)
                 (:constructor make-trustee (&key form type name))
                 (:constructor trustee (form type name)))
  (reserved pointer :initform &0)
  (multiple-trustee-operation
    (enum () :no-multiple-trustee-operation)
    :initform :no-multiple-trustee-operation)
  (form trustee-form)
  (type trustee-type)
  (name trustee-name :initform &0))

(declaim (notinline %trustee-writer))
(defun %trustee-writer (value pointer)
  (declare (type pointer pointer)
           (type trustee value))
  (setf (deref pointer 'trustee-type (offsetof 'trustee 'type))
        (trustee-type value)
        (deref pointer 'trustee-form (offsetof 'trustee 'form))
        (trustee-form value)
        (deref pointer 'trustee-name (offsetof 'trustee 'name))
        (trustee-name value)
        (deref pointer 'pointer)
        &0
        (deref pointer 'int (offsetof 'trustee 'multiple-trustee-operation))
        0)
  value)

(declaim (notinline %trustee-reader))
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

(define-enum (actrl-access-flags
               (:conc-name actrl-)
               (:base-type ulong))
  (:access-allowed 1)
  (:access-denied  2)
  (:audit-success  4)
  (:audit-failure  8))

(define-enum (actrl-standard-rights
               (:base-type ulong)
               (:conc-name actrl-))
  (:system-access  #x04000000)
  (:delete         #x08000000)
  (:read-control   #x10000000)
  (:change-access  #x20000000)
  (:synchronize    #x80000000)
  (:std-rights-all #xf8000000)
  (:std-rights-required #x78000000))

(define-enum (inheritance-flags
               (:base-type dword)
               (:conc-name nil))
  (:no-inheritance           #x0)
  (:container-inherit-ace    #x2)
  (:inherit-only-ace          #x8)
  (:no-propagate-inherit-ace #x4)
  (:object-inherit-ace       #x1)
  (:sub-containers-and-objects-inherit #x3)
  (:sub-containers-only-inherit #x2)
  (:sub-objects-only-inherit #x1))

(define-struct (actrl-access-entry
                 (:constructor make-actrl-ae)
                 (:conc-name actrl-ae-))
    "Contains access-control information for a specified trustee. "
  (trustee              trustee)
  (access-flags         actrl-access-flags)
  (access               actrl-standard-rights)
  (prov-specific-access dword)
  (inheritance          inheritance-flags)
  (inherit-property     (& tstring :in t)))

(define-struct (actrl-access-entry-list
                 (:constructor make-actrl-ael (&key list))
                 (:conc-name actrl-ael-)
                 (:reader %actrl-ael-reader)
                 (:writer %actrl-ael-writer))
    "Contains a list of access entries."
  (entries ulong)
  (list (& (~ actrl-access-entry) :in t)))

(defun %actrl-ael-reader (pointer out)
  (declare (type pointer pointer))
  (let* ((out (or out (make-actrl-ael)))
         (n (deref pointer 'ulong))
         (v (map-into (make-array n) #'make-actrl-ae))
         (pv (deref pointer '* (offsetof 'actrl-access-entry-list 'list))))
    (if (and (/= 0 n) (&? pv))
      (setf (actrl-ael-list out)
            (deref pv '(simple-array actrl-access-entry) 0 v)
            (actrl-ael-entries out) n)
      (setf (actrl-ael-list out) void
            (actrl-ael-entries out) 0))
    out))

(defun %actrl-ael-writer (value pointer)
  (declare (type pointer pointer)
           (type actrl-access-entry-list value))
  (let ((list (actrl-ael-list value)))
    (setf (deref pointer 'ulong)
          (if (voidp list) 0 (length (actrl-ael-list value)))
          (deref pointer
                 '(& (~ actrl-access-entry) :in t)
                 (offsetof 'actrl-access-entry-list 'list))
          list))
  value)

(defconstant actrl-access-not-protected 0)
(defconstant actrl-access-protected 1)

(define-struct (actrl-property-entry
                 (:constructor make-actrl-pe)
                 (:conc-name actrl-pe-))
    "Contains a list of access-control entries for an object or a specified property on an object. "
  (property (& (const tstring)))
  (access-entry-list (& actrl-access-entry-list :in t))
  (list-flags (enum (:base-type ulong)
                :not-protected
                :protected)))

(define-struct (actrl-access
                 (:constructor make-actrl-access (&key list))
                 (:reader %actrl-access-reader)
                 (:writer %actrl-access-writer))
    "Contains an array of access-control lists for an object and its properties."
  (entries ulong)
  (list (& (~ actrl-property-entry) :in t)))

(defun %actrl-access-reader (pointer out)
  (declare (type pointer pointer))
  (let* ((out (or out (make-actrl-access)))
         (n (deref pointer 'ulong))
         (v (map-into (make-array n) #'make-actrl-pe))
         (pv (deref pointer '* (offsetof 'actrl-access 'list))))
    (if (and (/= 0 n) (&? pv))
      (setf (actrl-access-list out)
            (deref pv '(simple-array actrl-property-entry) 0 v)
            (actrl-access-entries out) n)
      (setf (actrl-access-entries out) 0
            (actrl-access-list out) void))
    out))

(defun %actrl-access-writer (value pointer)
  (declare (type pointer pointer)
           (type actrl-access value))
  (let ((list (actrl-access-list value)))
    (setf (deref pointer 'ulong)
          (if (voidp list) 0 (length (actrl-access-list value)))
          (deref pointer
                 '(& (~ actrl-property-entry) :in t)
                 (offsetof 'actrl-access 'list))
          list))
  value)
