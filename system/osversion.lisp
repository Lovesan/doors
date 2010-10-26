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

(define-enum (version-suite (:conc-name ver-suite-)
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
               (:base-type byte))
  (:domain-controller #x00000002)
  (:server #x00000003)
  (:workstation #x00000001))

(define-struct (os-version-info-ex
                 (:constructor os-version-info-ex ())
                 (:conc-name osverinfo-))
  (size dword :initform (sizeof 'os-version-info-ex))
  (major-version dword)
  (minor-version dword)
  (build-number dword)
  (platform-id dword)
  (csd-version (tstring 128))
  (service-pack-major word)
  (service-pack-minor word)
  (suite-mask word)
  (product-type version-product-type)
  (reserved byte))

(define-external-function
    ("GetVersion" (:camel-case))
    (:stdcall kernel32)
  (dword rv (if (zerop rv)
              (error "Error requesting Windows version")
              rv)))

(load-time-value
  (defconstant winnt-version (let ((v (get-version)))
                               (logior (high-byte (low-word v))
                                       (ash (low-byte (low-word v)) 8)))))

(define-external-function
    (#+doors.unicode "GetVersionExW"
     #-doors.unicode "GetVersionExA"
                 get-version-ex)
    (:stdcall kernel32)
  (boolean rv (if rv
                version-info
                (error "Error requesting Windows NT version")))
  (version-info (& os-version-info-ex :inout)
                :aux (os-version-info-ex)))

(let ((info (get-version-ex)))
  (when info
    (pushnew (case (osverinfo-major-version info)
               (5 (case (osverinfo-minor-version info)
                    (0 :win2000)
                    (1 :winxp)
                    (2 (cond
                         ((eq (osverinfo-product-type info) :workstation)
                          :winxp64)
                         ((/= 0 (logand (osverinfo-suite-mask info)
                                        ver-suite-home-server))
                          :winhomeserver)
                         (T :winserver2003)))))
               (6 (case (osverinfo-minor-version info)
                    (0 (if (eq (osverinfo-product-type info)
                               :workstation)
                         :winvista
                         :winserver2008))
                    (1 (if (eq (osverinfo-product-type info)
                               :workstation)
                         :win7
                         :winserver2008r2))
                    (T :windows)))
               (T (error "Unsupported system")))
             *features*)))
