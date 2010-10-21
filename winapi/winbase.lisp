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

(declaim (inline make-short))
(defun make-short (a b)
  (lognot (logand #xFFFF
                  (lognot
                    (logior (logand a #xFF)
                            (ash (logand b #xFF) 8))))))

(declaim (inline make-word))
(defun make-word (a b)
  (logior (logand a #xFF)
          (ash (logand b #xFF) 8)))

(declaim (inline make-long))
(defun make-long (a b)
  (lognot (logand #xFFFFFFFF
                  (lognot
                    (logior (logand a #xFFFF)
                            (ash (logand b #xFFFF) 16))))))

(declaim (inline make-dword))
(defun make-dword (a b)
  (logand #xFFFFFFFF
          (logior (logand a #xFFFF)
                  (ash (logand b #xFFFF) 16))))

(declaim (inline make-long-long))
(defun make-long-long (a b)
  (lognot (logand #xFFFFFFFFFFFFFFFF
                  (lognot
                    (logior (logand a #xFFFFFFFF)
                            (ash (logand b #xFFFFFFFF) 32))))))

(declaim (inline make-qword))
(defun make-qword (a b)
  (logand #xFFFFFFFFFFFFFFFF
          (logior (logand a #xFFFFFFFF)
                  (ash (logand b #xFFFFFFFF) 32))))

(declaim (inline low-dword))
(defun low-dword (x)
  (logand x #xFFFFFFFF))

(declaim (inline high-dword))
(defun high-dword (x)
  (logand (ash x -32) #xFFFFFFFF))

(declaim (inline low-word))
(defun low-word (x)
  (logand x #xFFFF))

(declaim (inline high-word))
(defun high-word (x)
  (logand (ash x -16) #xFFFF))

(declaim (inline low-byte))
(defun low-byte (x)
  (logand x #xFF))

(declaim (inline high-byte))
(defun high-byte (x)
  (logand (ash x -8) #xFF))

(define-struct (rect
                 (:constructor rect (left top right bottom)))
  (left long)
  (top long)
  (right long)
  (bottom long))

(define-struct (point
                 (:constructor point (x y))
                 (:conc-name pt-))
  (x long)
  (y long))

(define-struct (size
                 (:constructor size (cx cy)))
  (cx long)
  (cy long))

(define-struct (point-s
                 (:constructor point-s (x y))
                 (:conc-name pt-s-))
  (x short)
  (y short))

(define-struct (filetime
                 (:constructor filetime (low-date-time
                                         high-date-time)))
  (low-date-time dword)
  (high-date-time dword))
