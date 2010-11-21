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

(in-package #:doors.com.examples)

(define-interface hello-world
    ((iid-hello-world
       #xF9210244 #x38D1 #x49C0
       #xA8 #x48 #x68 #x4E #xDD #x3D #xBF #xF0)
     unknown)
  (hello-world (hresult)
      (string (& wstring) :optional "Hello, world!")))

(define-guid clsid-hello-world
  #xDF748DA7 #xBCB9 #x4F67
  #x8D #x32 #xF9 #xAA #x1A #xAA #x3A #xBF)

(defclass hello-world-object (com-object)
  ((locked :initform nil :accessor hwo-locked-p))
  (:metaclass com-class)
  (:clsid . clsid-hello-world))

(defmethod hello-world ((object hello-world-object)
                        &optional (string "Hello, world!"))
  (write-line string)
  (values nil string))

(defmethod lock-server ((object hello-world-object) lock)
  (if (hwo-locked-p object)
    (if lock
      (warn 'windows-status :code status-false)
      (progn (release object)
             (setf (hwo-locked-p object) nil)))
    (if lock
      (progn (add-ref object)
             (setf (hwo-locked-p object) t))
      (warn 'windows-status :code status-false)))
  (values nil lock))

(defmethod create-instance
    ((object hello-world-object) iid &optional outer)
  (if outer
    (error 'com-error :code error-not-implemented)
    (progn
      (unless (typep iid 'com-interface-class)
        (setf iid (find-interface-class iid)))
      (unless (member (class-name iid)
                      '(unknown class-factory hello-world))
        (error 'com-error :code error-no-interface))
      (let ((object (make-instance 'hello-world-object)))
        (values nil outer iid (acquire-interface object iid))))))

(defun register-server ()
  (handler-bind
    ((windows-status #'muffle-warning))
    (initialize))
  (let* ((object (make-instance 'hello-world-object))
         (unknown (acquire-interface object 'unknown t))
         (register (register-class-object 'hello-world-object unknown
                                          :server :multiple-use)))
    (values register object)))
