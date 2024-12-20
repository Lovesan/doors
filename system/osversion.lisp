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

(in-package #:doors)

(define-enum (version-suite (:conc-name ver-suite-)
                            (:list t)
                            (:base-type word))
  (:backoffice #x00000004)
  (:blade #x00000400)
  (:compute-server #x00004000)
  (:datacenter #x00000080)
  (:enterprise #x00000002)
  (:embedded-nt #x00000040)
  (:personal #x00000200)
  (:single-user-ts #x00000100)
  (:small-business #x00000001)
  (:small-business-restricted #x00000020)
  (:storage-server #x00002000)
  (:terminal #x00000010)
  (:communications #x00000008)
  (:embedded-restricted #x00000800)
  (:security-appliance #x00001000)
  (:home-server #x00008000))

(defconstant ver-server-nt #x80000000)
(defconstant ver-workstation-nt #x40000000)

(define-enum (version-product-type
               (:conc-name ver-nt-)
               (:list t)
               (:base-type byte))
  (:domain-controller #x00000002)
  (:server #x00000003)
  (:workstation #x00000001))

(define-struct (os-version-info
                 (:constructor make-os-version-info
                               (&key major-version minor-version
                                build-number platform-id
                                csd-version
                                service-pack-major
                                service-pack-minor
                                suite-mask
                                product-type))
                 (:conc-name osverinfo-))
  (size dword :initform (sizeof 'os-version-info))
  (major-version dword)
  (minor-version dword)
  (build-number dword)
  (platform-id (enum (:base-type dword)
                     (:nt 2)))
  (csd-version (tstring 128))
  (service-pack-major word)
  (service-pack-minor word)
  (suite-mask version-suite)
  (product-type version-product-type)
  (reserved byte))

(define-external-function
    ("GetVersion" os-version)
    (:stdcall kernel32)
  (dword rv (if (zerop rv)
              (error "Error requesting Windows version")
              rv)))

(define-symbol-macro os-version (os-version))

(load-time-value
  (defconstant winnt-version (let ((v os-version))
                               (logior (high-byte (low-word v))
                                       (ash (low-byte (low-word v)) 8)))))

(define-external-function
    (#+doors.unicode "GetVersionExW"
     #-doors.unicode "GetVersionExA"
                 os-version*)
    (:stdcall kernel32)
  (bool rv (if rv
                version-info
                (error "Error requesting Windows NT version")))
  (version-info (& os-version-info :inout) :aux))

(define-symbol-macro os-version* (os-version*))

(let ((info os-version*))
  (pushnew (case (osverinfo-major-version info)
             (5 (case (osverinfo-minor-version info)
                  (0 :win2000)
                  (1 :winxp)
                  (2 (cond
                       ((member :workstation (osverinfo-product-type info))
                        :winxp64)
                       ((member :home-server (osverinfo-suite-mask info))
                        :winhomeserver)
                       (T :winserver2003)))))
             (6 (case (osverinfo-minor-version info)
                  (0 (if (member :workstation (osverinfo-product-type info))
                       :winvista
                       :winserver2008))
                  (1 (if (member :workstation (osverinfo-product-type info))
                       :win7
                       :winserver2008r2))
                  (2 (if (member :workstation (osverinfo-product-type info))
                       :win8
                       :winserver2012))
                  (3 (if (member :workstation (osverinfo-product-type info))
                       :win8.1
                       :winserver2012r2))
                  (T :windows)))
             (10 (case (osverinfo-minor-version info)
                   (0 (if (member :workstation (osverinfo-product-type info))
                        :win10
                        :winserver2016))))
             (T (error "Unsupported system")))
           *features*))
