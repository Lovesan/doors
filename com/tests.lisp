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

(defalias float4 () '(simple-array float (4)))
(defun float4 (x y z w)
  (make-array 4 :element-type 'single
    :initial-contents (list x y z w)))

(define-interface vector-adder ()
  (add-vectors (void rv out)
    (out (& float4 :inout) :optional)
    (a (& float4))
    (b (& float4))))

(defclass vector-adder-object (com-object)
  ())

(defmethod add-vectors ((object vector-adder-object) a b
                        &optional (out (float4 0.0 0.0 0.0 0.0)))
  (values void (map-into out #'+ a b) a b))

(defun vector-adder-test ()
  (let* ((object (make-instance 'vector-adder-object))
         (interface (acquire-interface object 'vector-adder t))
         (v1 (float4 1.0 2.0 3.0 4.0))
         (v2 (float4 5.0 6.0 7.0 8.0)))
    (finalize interface (lambda ()
                          (write-line "ADDER interface disposing"
                                      *error-output*)))
    (finalize object (lambda ()
                       (write-line "ADDER-OBJECT object disposing"
                                   *error-output*)))
    (add-vectors interface v1 v2 v1)
    (every (complement #'null)
           (map 'vector #'=
             (add-vectors interface v1 v2)
             #(11.0 14.0 17.0 20.0)))))

(define-iid iid-stack
    #xCAFEBABE #xF001 #xBABE 0 0 0 0 #x0B #xAD #xF0 #x0D)

(define-interface stack (iid-stack unknown)
  (stack-push (void) (value int))
  (stack-pop  (hresult rv value) (value (& int :out) :aux)))

(defclass stack-object (com-object)
  ((stack :initform '()
          :accessor stack-object-stack)))

(defmethod stack-push ((object stack-object) value)
  (push value (stack-object-stack object))
  (values void value))

(defmethod stack-pop ((object stack-object))
  (if (endp (stack-object-stack object))
    (error 'windows-error)
    (values nil (pop (stack-object-stack object)))))

(defun stack-test ()
  (let* ((object (make-instance 'stack-object)))
    (with-interfaces ((unknown (acquire-interface object 'unknown))
                      (stack (query-interface unknown iid-stack)))
      (add-ref stack)
      (finalize stack (lambda ()
                        (write-line "STACK interface disposing"
                                    *error-output*)))
      (finalize object (lambda ()
                         (write-line "STACK-OBJECT object disposing"
                                     *error-output*)))
      (stack-push stack 123)
      (and (eql 123 (stack-pop stack))
           (handler-case
               (progn (stack-pop stack) nil)
             (error (error)
               (and (typep error 'windows-error)
                    (= (windows-error-code error)
                       error-failure))))))))
