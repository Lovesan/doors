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

(defvar *registered-results* (make-hash-table :test #'eql))
(defvar *result-descriptions* (make-hash-table :test #'eql))

(define-enum facility
  :null
  :rpc
  :dispatch
  :storage
  :interface
  (:win32 7)
  :windows
  :security
  (:sspi 9)
  :control
  :certification
  :internet
  :media-server
  :msmq
  :setup-api
  :smart-card
  :com+
  :aaf
  :urt
  :acs
  :direct-play
  :umi
  :sxs
  :windows-ce
  :http
  :usermode-common-logging
  :usermode-filter-manager  
  (:background-copy 32)
  :configuration
  :state-management
  :meta-directory
  :windows-update
  :directory-service
  :graphics
  :shell
  :tpm-services
  :tpm-software
  (:pla 48)
  :fve
  :fwp
  :winrm
  :ndis
  :usermode-virtualization
  :usermode-volmgr
  :bcd
  :usermode-vhd
  :sdiag
  :web-services
  (:windows-defender 80)
  :opc)

(declaim (inline make-hresult))
(defun make-hresult (errorp facility code)
  (logior (if errorp #x80000000 #x00000000)
          (ash (logand (convert facility 'facility) #x7FF) 16)
          (logand #xFFFF code)))

(defun hresult-from-win32 (error-code)
  (make-hresult (/= 0 error-code)
    :win32 error-code))

(defun hresult-from-nt (error-code)
  (logior (logand #xFFFFFFFF error-code)
          #x10000000))

(declaim (inline hresult-error-p))
(defun hresult-error-p (hresult)
  (declare (type dword hresult))
  (logbitp 31 hresult))

(declaim (inline hresult-facility))
(defun hresult-facility (hresult)
  (declare (type dword hresult))
  (translate (logand #x7FF (ash hresult -16))
             'facility))

(declaim (inline hresult-code))
(defun hresult-code (hresult)
  (declare (type dword hresult))
  (logand #xFFFF hresult))

(define-condition windows-condition (condition)
  ((code :initarg :code
         :accessor windows-condition-code
         :initform 0))
  (:report (lambda (condition stream)
             (let ((code (windows-condition-code condition)))
               (format stream
                       "Status: ~:[Success~;Failure~] ~_Facility: ~a ~_Code: ~4,'0X~:[~; ~_~:*~a~]"
                       (hresult-error-p code)
                       (if (logbitp 30 code)
                         "NT"
                         (hresult-facility code))
                       (hresult-code code)
                       (or (let ((code (if (eq :win32 (hresult-facility code))
                                         (hresult-code code)
                                         code)))
                             (with-pointer (pp &0 'pointer)
                               (when (/= 0 (external-function-call
                                             #+doors.unicode "FormatMessageW"
                                             #-doors.unicode "FormatMessageA"
                                             ((:stdcall kernel32)
                                              (dword)
                                              (dword flags :aux #x1300)
                                              (pointer source :aux)
                                              (dword message-id :aux code)
                                              (dword language-id :aux
                                                     #+doors.unicode 0
                                                     #-doors.unicode #x00000409)
                                              (pointer buffer :aux pp)
                                              (dword size :aux)
                                              (pointer args :aux))))
                                 (unwind-protect
                                     (deref pp '(& tstring))
                                   (external-function-call
                                     "LocalFree"
                                     ((:stdcall kernel32)
                                      (void)
                                      (pointer))
                                     (deref pp '*))))))
                           (gethash code *result-descriptions*)))
               condition))))

(defmacro define-results (name (&rest superclasses)
                                     (&rest slots)
                               (&rest codes)
                               &rest options)
  (let* ((conc-name-spec (assoc :conc-name options))
         (conc-name (if conc-name-spec
                      (or (second conc-name-spec) "")
                      (format nil "~a-" name))))
    `(progn
       ,@(loop :for code-spec :in codes
           :append (destructuring-bind
                       (code-name code-value &optional (code-description nil desc-p))
                       code-spec
                     (check-type code-name symbol)
                     (when desc-p
                       (check-type code-description string))
                     (let ((value (when (constantp code-value)
                                    (eval code-value))))
                       (unless (integerp value)
                         (error "Invalid result code value: ~s" code-spec))
                     `((defconstant ,(intern (format nil "~a~a" (string conc-name)
                                                     code-name))
                         ,value)
                       (eval-when (:compile-toplevel :load-toplevel :execute)
                         (setf (gethash ,value *registered-results*)
                               ',name
                               (gethash ,value *result-descriptions*)
                               ,code-description))))))
       (define-condition ,name (,@superclasses windows-condition)
         ,slots
         ,@(remove :conc-name options :key #'car))
       ',name)))

(deftype hresult () '(or null windows-condition))

(define-immediate-type hresult-type ()
  ()
  (:base-type dword)
  (:simple-parser hresult)
  (:lisp-type (type) '(or null windows-condition))
  (:prototype (type) nil)
  (:prototype-expansion (type) nil)
  (:converter (value type)
    (if (null value)
      0
      (slot-value value 'code)))
  (:translator (value type)
    (if (zerop value)
      nil
      (let* ((errorp (logbitp 31 value))
             (condition-name (gethash value *registered-results*))
             (condition (make-condition
                          (or condition-name
                              (if errorp 'windows-error 'windows-status))
                          :code value)))
        (if errorp
          (error condition)
          (warn condition)))))
  (:converter-expansion (value type)
    (once-only (value)
      `(if (null ,value)
         0
         (slot-value ,value 'code))))
  (:translator-expansion (value type)
    (with-gensyms (code errorp condition-name condition)
      `(let ((,code ,value))
         (declare (type dword ,code))
         (if (zerop ,code)
           nil
           (let* ((,errorp (logbitp 31 ,code))
                  (,condition-name (gethash ,code *registered-results*))
                  (,condition (make-condition
                                (or ,condition-name
                                    (if ,errorp 'windows-error 'windows-status))
                                :code ,code)))
             (if ,errorp
               (error ,condition)
               (warn ,condition)))))))
  (:cleaner-expansion (pointer value type) nil)
  (:allocator-expansion (value type) `(alloc 'dword))
  (:deallocator-expansion (pointer type) `(free ,pointer 'dword)))
