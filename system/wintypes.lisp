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

(defalias word () 'uint16)
(deftype word () 'uint16)

(defalias dword () 'uint32)
(deftype dword () 'uint32)

(defalias qword () 'uint64)
(deftype qword () 'uint64)

(defalias wparam () 'uint-ptr)
(deftype wparam () 'uint-ptr)
(defalias lparam () 'int-ptr)
(deftype lparam () 'int-ptr)
(defalias lresult () 'int-ptr)
(deftype lresult () 'int-ptr)

(defalias long-ptr () 'int-ptr)
(deftype long-ptr () 'int-ptr)
(defalias ulong-ptr () 'uint-ptr)
(deftype ulong-ptr () 'uint-ptr)

(defalias atom () 'word)
(defconstant invalid-atom 0)
(declaim (inline valid-atom-p))
(defun valid-atom-p (atom)
  (declare (type word atom))
  (not (zerop atom)))

(deftype handle () '(or null pointer))

(define-immediate-type handle-type ()
  ()
  (:simple-parser handle)
  (:base-type pointer)
  (:lisp-type (type) 'handle)
  (:prototype (type) nil)
  (:prototype-expansion (type) nil)
  (:converter (value type)
    (or value &0))
  (:translator (value type)
    (and (&? value) value))
  (:converter-expansion (value type)
    `(or ,value &0))
  (:translator-expansion (value type)
    (with-gensyms (handle)
     `(let ((,handle ,value))
        (declare (type pointer ,handle))
        (and (&? ,handle) ,handle))))
  (:allocator-expansion (value type)
    `(alloc '*))
  (:deallocator-expansion (pointer type)
    `(free ,pointer '*))
  (:cleaner-expansion (pointer value type)
    nil))

(define-symbol-macro invalid-handle-value (& #xFFFFFFFF))
(declaim (inline valid-handle-p))
(defun valid-handle-p (handle)
  (declare (type handle handle))
  (or (null handle)
      (not (&= handle invalid-handle-value))))

(defalias astring (&optional length)
  `(string :encoding :ascii
           :byte-length ,(if length
                           (* length (sizeof 'char))
                           nil)))
(defalias wstring (&optional length)
  `(string :encoding :utf-16le
           :byte-length ,(if length
                           (* length (sizeof 'wchar))
                           nil)))

(defalias tstring (&optional length)
  #+doors.unicode `(wstring ,length)
  #-doors.unicode `(astring ,length))

(defalias tchar ()
  #+doors.unicode 'wchar
  #-doors.unicode 'char)


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

(defconstant unicode-string-max-bytes 65534)
(defconstant unicode-string-max-chars 32767)
