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
  (:server                 #x19)
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
  (:all                    #b11011)
  (:ps-dll                 #x80000000))
