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

(define-struct (auth-info)
    "Contains the authentication settings used while making a remote activation request."
  (authn-svc (enum (:base-type dword)
               (:none 0)
               (:dce-private 1)
               (:dce-public  2)
               (:dec-public  4)
               (:gss-negotiate 9)
               (:winnt 10)
               (:gss-schannell 14)
               (:gss-kerberos  16)
               (:dpa 17)
               (:msn 18)
               (:kernel 20)
               (:digest 21)
               (:nego-extender 30)
               (:pku2u 31)
               (:mq 100)
               (:default #xffffffff)))
  (authz-svc (enum (:base-type dword)
               :none
               :name
               :dce
               (:default #xFFFFFFFF)))
  (server-principal-name (& wstring))
  (authn-level (enum (:base-type dword)
                 (:default 0)
                 (:none 1)
                 (:connect 2)
                 (:call 3)
                 (:level-pkt 4)
                 (:pkt-integrity 5)
                 (:pkt-privacy 6)))
  (impersonation-level (enum (:base-type dword)
                         (:default 0)
                         (:anonymous 1)
                         (:identity 2)
                         (:impersonate 3)
                         (:delegate 4)))
  (auth-identity-data pointer)
  (capabilities (enum (:base-type dword)
                  :none
                  :mutual-auth)))

(define-struct (server-info
                 (:constructor make-server-info (&key name auth-info)))
    "Identifies a remote computer resource to the activation functions. "
  (reserved1 dword)
  (name (& wstring))
  (auth-info (& auth-info :in t))
  (reserved2 dword))
