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

(define-enum (com-rights
               (:base-type dword))
  (:execute         1)
  (:execute-local   2)
  (:execute-remote  4)
  (:activate-local  8)
  (:activate-remote 16))

(define-enum (class-object-registration-flags
               (:base-type dword)
               (:conc-name regcls-)
               (:list t))
  (:single-use     0)
  (:multiple-use   1)
  (:multi-separate 2)
  (:suspended      4)
  (:surrogate      8))

(define-enum (class-context-flags
               (:base-type dword)
               (:conc-name clsctx-)
               (:list t))
  (:inproc-server          #x1)
  (:inproc-handler         #x2)
  (:inproc                 #x3)
  (:local-server           #x4)
  (:remote-server          #x10)
  (:server                 #x15)
  (:no-code-download       #x400)
  (:no-custom-marshal      #x1000)
  (:enable-code-download   #x2000)
  (:no-failure-log         #x4000)
  (:disable-aaa            #x8000)
  (:enable-aaa             #x10000)
  (:from-default-context   #x20000)
  (:activate-32-bit-server #x40000)
  (:activate-64-bit-server #x80000)
  (:enable-cloaking        #x100000)
  (:all                    #x17)
  (:ps-dll                 #x80000000))

(define-enum (com-init-flags
               (:base-type dword)
               (:list t)
               (:conc-name coinit-))
  (:multithreaded       0)
  (:apartment-threaded 2)
  (:disable-ole1-dde    4)
  (:speed-over-memory   8))

(define-enum (bind-flags
               (:conc-name bind-)
               (:base-type dword)
               (:list t))
  (:may-bother-user 1)
  (:just-test-existence 2))

(define-enum (stgm-flags
               (:base-type dword)
               (:conc-name stgm-))
  (:read #x00000000)
  (:write #x00000001)
  (:read-write #x00000002)
  (:share-deny-none #x00000040)
  (:share-deny-read #x00000030)
  (:share-deny-write #x00000020)
  (:share-exclusive #x00000010)
  (:priority #x00040000)
  (:create #x00001000)
  (:convert #x00020000)
  (:fail-if-there #x00000000)
  (:direct #x00000000)
  (:transacted #x00010000)
  (:no-scratch #x00100000)
  (:no-snapshot #x00200000)
  (:simple #x08000000)
  (:direct-swmr #x00400000)
  (:delete-on-release #x04000000))
