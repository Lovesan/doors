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

(in-package #:cl-user)

(defpackage #:doors.security
  (:use #:cl #:alexandria #:virgil #:doors #:doors.com)
  (:export
   
    ;;authentication stuff
    #:string*
    #:astring*
    #:wstring*
    #:tstring*
    
    ;;authorization stuff
    #:security-attributes
    #:security-atributes-descriptor
    #:security-attributes-inherit-handle
    #:luid
    #:luid-low-part
    #:luid-high-part
    #:trustee
    #:trustee-name
    #:trustee-form
    #:trustee-type
    #:trustee-is-sid
    #:trustee-is-name
    #:trustee-is-bad-form
    #:trustee-is-objects-and-sid
    #:trustee-is-objects-and-name
    #:trustee-is-unknown
    #:trustee-is-user
    #:trustee-is-group
    #:trustee-is-domain
    #:trustee-is-alias
    #:trustee-is-well-known-group
    #:trustee-is-deleted
    #:trustee-is-invalid
    #:trustee-is-compute
    #:se-object-type
    #:se-unknown-object-type
    #:se-file-object
    #:se-service
    #:se-printer
    #:se-registry-key
    #:se-lmshare
    #:se-kernel-object
    #:se-window-object
    #:se-ds-object
    #:se-ds-object-all
    #:se-provider-defined-object
    #:se-wmi-guid-object
    #:se-registry-wow64-32-key
    #:objects-and-name
    #:make-objects-and-name
    #:objects-and-name
    #:objects-and-name-objects-present
    #:objects-and-name-object-type
    #:objects-and-name-object-type-name
    #:objects-and-name-inherited-object-type-name
    #:objects-and-name-name
    #:object-and-sid
    #:make-objects-and-sid
    #:objects-and-sid-objects-present
    #:objects-and-sid-object-type-guid
    #:objects-and-sid-inherited-object-type-guid
    #:objects-and-sid-sid
    ))
