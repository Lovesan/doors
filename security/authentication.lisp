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

(in-package #:doors.security)

(define-struct %string
  (length ushort)
  (maximum-length ushort)
  (buffer pointer))

(defun %read-string (pointer out encoding)
  (declare (type pointer pointer))
  (let ((len (deref pointer 'ushort)))
    (read-cstring (deref pointer 'pointer (offsetof '%string
                                                    'buffer))
                  :encoding encoding
                  :byte-length len
                  :out out)))

(defun %write-string (value pointer encoding)
  (declare (type pointer pointer))
  (let ((len (min (cstring-size value :encoding encoding)
                  (deref pointer 'ushort (offsetof '%string
                                                   'maximum-length))))
        (buffer (deref pointer 'pointer (offsetof '%string
                                                  'buffer))))
    (declare (type ushort len))
    (setf (deref pointer 'ushort) len)
    (write-cstring value
                   buffer
                   :encoding encoding
                   :byte-length len)))

(defun %string-size (value encoding)
  (multiple-value-bind
      (data-len bom-len) (cstring-size value :encoding encoding)
    (the ushort (+ data-len bom-len))))

(defun %allocate-string (value encoding)
  (let* ((maxlen (%string-size value encoding))
         (p (raw-alloc (sizeof '%string)))
         (buffer (raw-alloc maxlen)))
    (setf (deref p 'ushort) 0
          (deref p 'ushort (offsetof '%string
                                     'maximum-length)) maxlen
          (deref p 'pointer (offsetof '%string
                                      'buffer)) buffer)
    p))

(defun %free-string (pointer)
  (raw-free (deref pointer 'pointer (offsetof '%string 'buffer)))
  (raw-free pointer))

(define-translatable-type %string-type ()
  ((encoding :initform :ascii :initarg :encoding
             :accessor %string-encoding))
  (:fixed-size (type)
    (sizeof '%string))
  (:align (type)
    (length
      (babel-encodings:enc-nul-encoding
        (babel-encodings:get-character-encoding
          (%string-encoding type)))))
  (:prototype (type) "")
  (:prototype-expansion (type) "")
  (:lisp-type (type) 'string)
  (:allocator (value type)
    (%allocate-string value (%string-encoding type)))
  (:allocator-expansion (value type)
    `(%allocate-string ,value ',(%string-encoding type)))
  (:deallocator (pointer type)
    (%free-string pointer))
  (:deallocator-expansion (pointer type)
    `(%free-string ,pointer))
  (:cleaner (pointer value type) nil)
  (:cleaner-expansion (pointer value type) nil)
  (:reader (pointer out type)
    (%read-string pointer out (%string-encoding type)))
  (:reader-expansion (pointer out type)
    `(%read-string ,pointer ,out ',(%string-encoding type)))
  (:writer (value pointer type)
    (%write-string value pointer (%string-encoding type)))
  (:writer-expansion (value pointer type)
    `(%write-string ,value ,pointer ',(%string-encoding type)))
  (:reference-dynamic-extent-expansion
    (var size-var value-var body mode type)
    (with-gensyms (buffer size pointer)
      `(with-raw-pointer (,pointer ,(sizeof '%string) ,size-var)
         (let* ((,size (%string-size ,value-var ',(%string-encoding type)))
                (,buffer (raw-alloc ,size))
                (,var ,pointer))
           (unwind-protect
               (progn (setf (deref ,pointer 'pointer (offsetof '%string
                                                               'buffer))
                            ,buffer
                            (deref ,pointer 'ushort (offsetof '%string
                                                               'maximum-length))
                            ,size)
                      nil
                      ,(ecase mode
                         (:in `(progn (%write-string ,value-var ,pointer
                                                     ',(%string-encoding type))
                                      nil
                                      ,@body))
                         (:out `(prog1 (progn ,@body)
                                 (%read-string ,pointer ,value-var
                                               ',(%string-encoding type))))
                         (:inout `(prog1 (progn
                                           (%write-string ,value-var ,pointer
                                                          ',(%string-encoding type))
                                           nil
                                           ,@body)
                                   (%read-string ,pointer ,value-var
                                                 ',(%string-encoding type))))))
             (raw-free ,buffer)))))))

(define-type-parser string* (&optional (encoding :ascii))
  (check-type encoding keyword)
  (make-instance '%string-type :encoding encoding))

(defmethod unparse-type ((type %string-type))
  `(string* ,(%string-encoding type)))

(defalias astring* () '(string* :ascii))
(defalias wstring* () '(string* :utf-16le))
(defalias tstring* () #+doors.unicode 'wstring*
                      #-doors.unicode 'astring*)

(define-struct (auth-identity)
    "Contains a user name and password."
  (user (& tstring))
  (user-length ulong)
  (domain (& tstring))
  (domain-length ulong)
  (password (& tstring))
  (password-length ulong)
  (flags (enum (:base-type ulong)
           (:ansi 1)
           (:unicode 2))
         :initform #+doors.unicode :unicode
                   #-doors.unicode :ansi))
