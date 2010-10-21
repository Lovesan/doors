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

(defpackage #:doors.com
  (:use #:cl #:alexandria #:trivial-garbage #:virgil #:doors)
  (:export
    
    ;;com-interface related stuff
    #:com-interface-class
    #:com-interface
    #:find-interface-class
    #:find-interface-class-by-iid
    #:com-interface-pointer
    #:com-interface-method-pointer
    #:translate-interface
    #:convert-interface
    #:define-interface
    #:define-interface-method
    
    ;;com-object related stuff
    #:com-object
    #:acquire-interface
    
    ;;IUnknown
    #:iid-unknown
    #:unknown
    #:query-interface
    #:add-ref
    #:release
    #:known-iid
    #:with-interface
    #:with-interfaces
    ))
