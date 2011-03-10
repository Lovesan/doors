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

(in-package #:doors.com.examples)

(closer-mop:defclass factory-class (com-class)
  ((server-atom :initform nil))
  (:interfaces class-factory)
  (:metaclass com-class))

(closer-mop:defmethod create-instance ((class factory-class) iid &optional outer)
  (if outer
    (error 'com-error :code error-not-implemented)
    (let ((object (make-instance class)))
      (values nil outer iid (acquire-interface object iid)))))

(closer-mop:defmethod lock-server ((class factory-class) lock)
  (if lock
    (add-ref-server-process)
    (when (zerop (release-server-process))
      (post-quit-message)))
  (values nil lock))

(define-interface hello-world
    ("{F9210244-38D1-49C0-A848-684EDD3DBFF0}" unknown)
  (hello-world (hresult)
      (string (& wstring) :optional "Hello, world!")))

(closer-mop:defclass hello-world-object (com-object)
  ()
  (:metaclass factory-class)
  (:interfaces hello-world)
  (:clsid . "{DF748DA7-BCB9-4F67-8D32-F9AA1AAA3ABF}"))

(closer-mop:defmethod hello-world ((object hello-world-object)
                        &optional (string "Hello, world!"))
  (write-line string)
  (values nil string))

(defmethod add-ref :after ((object hello-world-object))
  (add-ref-server-process))

(defmethod release :after ((object hello-world-object))
  (when (zerop (release-server-process))
    (post-quit-message)))

(defun register-server ()
  (let ((class (find-class 'hello-world-object)))
    (when (slot-value class 'server-atom)
      (ignore-errors (revoke-class-object (slot-value class 'server-atom))))
    (setf (slot-value class 'server-atom)
          (register-class-object class
                                 '(:inproc-server :local-server)
                                 '(:multiple-use :suspended))))
  (resume-class-objects))

(closer-mop:defclass hello-world-wrapper ()
  ()
  (:metaclass com-wrapper-class)
  (:interfaces hello-world)
  (:clsid . "{DF748DA7-BCB9-4F67-8D32-F9AA1AAA3ABF}"))

(defun run-server ()
  (initialize-com)
  (loop :with atom = (prog1
                      (register-class-object 'hello-world-object
                                             :local-server
                                             '(:multiple-use :suspended))
                      (resume-class-objects))
    :for msg = (get-message) :until (null msg)
    :do (dispatch-message msg)
    :finally (revoke-class-object atom))
  (uninitialize-com))
