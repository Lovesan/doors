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

(defun normalize-arg-spec (arg-spec)
  (destructuring-bind
      (name typespec &optional (kind :primary) (initform nil initform-p))
      arg-spec
    (list arg-spec typespec name kind initform initform-p)))

(defun parse-args (args)
  (loop :with names = '()
    :for (original typespec name kind initform initform-p)
    :in (mapcar #'normalize-arg-spec args)
    :do (cond
          ((member name names)
           (error "Duplicate argument names exist: ~s" args))
          ((not (member kind '(:primary :key :aux :optional)))
           (error "Invalid argument kind: ~s in ~s"
                  kind original))
          ((and (eq kind :primary) initform-p)
           (error "Initforms for :primary arguments are not allowed ~s"
                  original))
          (T (push name names)))
    :if (eq kind :primary) :collect name :into primary
    :if (eq kind :key) :collect name :into key
    :if (eq kind :optional) :collect name :into optional
    :if (eq kind :aux) :collect name :into aux
    :collect (list typespec name :aux name) :into normalized
    :collect (let ((type (parse-typespec typespec)))
               (list name
                     (lisp-type type)
                     (if (or initform-p (eq kind :primary))
                       initform
                       (expand-prototype type))))
    :into types
    :finally (if (and key optional)
               (error "~s and ~s arguments in the same argument list are not allowed ~s"
                      :key :optional args)
               (return (values normalized types primary key optional aux)))))

(defun make-method-arg-group (types group group-name)
  `(,group-name ,@(mapcar (lambda (name)
                            (list name (third (assoc name types))))
                    group)))

(defun make-internal-name (&rest args)
  (intern (format nil "~{~a~^::~}" args) :doors.com))

(defun make-defmethod-call-form (method-name this-var primary optional key)
  `(funcall
     #',method-name
     ,@(if (consp method-name)
         `(,(car primary)
           (gethash (&& ,this-var) *pointer-to-object-mapping*)
           ,@(rest primary))
         `((gethash (&& ,this-var) *pointer-to-object-mapping*)
           ,@primary))
     ,@optional
     ,@(loop :for name :in key :nconc
         (list (intern (string name) :keyword) name))))

(defun parse-method-spec (interface-name vtable-name method-name vtable-index method-spec)
  (destructuring-bind
      ((return-type &optional (return-value-name (gensym))
                    (result-form return-value-name))
       &rest doc-and-args)
      method-spec
    (check-type method-name (or symbol (cons (eql setf) (cons symbol null))))
    (check-type return-value-name symbol)
    (let ((doc (if (stringp (car doc-and-args))
                 (car doc-and-args)
                 nil))
          (args (if (stringp (car doc-and-args))
                  (rest doc-and-args)
                  doc-and-args))
          (this-var (intern (string 'this)))
          (rv-var (gensym (string 'return-value))))
      (multiple-value-bind
          (normalized types primary key optional aux) (parse-args args)
        `((progn
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (closer-mop:ensure-generic-function ',method-name
                :generic-function-class 'com-generic-function
                :lambda-list '(,@(if (consp method-name)
                                   `(,(car primary)
                                     ,this-var
                                     ,@(rest primary))
                                   `(,this-var ,@primary))
                               ,@(when optional `(&optional ,@optional))
                               ,@(when key `(&key ,@key)))
                ,@(when doc `(:documentation ,doc))))
            ,@(flet ((method-lambda-list (wrapper)
                       `(,@(if (consp method-name)
                             `(,(car primary)
                               ,(if wrapper
                                  this-var
                                  `(,this-var ,interface-name))
                               ,@(rest primary))
                             `(,(if wrapper
                                  this-var
                                  `(,this-var ,interface-name))
                               ,@primary))
                            ,@(unless (null key)
                                (make-method-arg-group types key '&key))
                            ,@(unless (null optional)
                                (make-method-arg-group types optional '&optional))
                            ,@(unless (null aux)
                                (make-method-arg-group types aux '&aux))))
                     (arg-type-decls ()
                       (loop :for (name lisp-type initform) :in types
                         :collect `(type ,lisp-type ,name)))
                     (callout-form (wrapper)
                       `(external-pointer-call
                          (deref (&+ (deref ,(if wrapper
                                               this-var
                                               `(com-interface-pointer ,this-var))
                                            'pointer)
                                  ,vtable-index
                                  'pointer)
                                 'pointer)
                          ((:stdcall)
                           (,return-type ,return-value-name ,result-form)
                           (,(if wrapper 'pointer interface-name)
                              ,this-var :aux ,this-var)
                           ,@normalized))))
                `((closer-mop:defmethod ,method-name ,(method-lambda-list nil)
                    (declare (type ,interface-name ,this-var)
                             ,@(arg-type-decls))
                    ,(callout-form nil))
                  ,(with-gensyms (method-args next-method-list)
                     `(eval-when (:compile-toplevel :load-toplevel :execute)
                        (setf (gethash ',method-name (%interface-class-wrapper-functions
                                                       (find-interface-class ',interface-name)))
                              (cons
                                #-ccl
                                (lambda (,method-args ,next-method-list)
                                  (declare (ignore ,next-method-list)
                                           (type cons ,method-args))
                                  (destructuring-bind ,(method-lambda-list t) ,method-args
                                    (declare (type com-wrapper ,this-var)
                                             ,@(arg-type-decls))
                                    (let ((,this-var
                                            (or (gethash ',interface-name
                                                         (%wrapper-interface-pointers ,this-var))
                                                (error 'com-error :code error-not-implemented))))
                                      (declare (type pointer ,this-var))
                                      ,(callout-form t))))
                                #+ccl
                                (lambda ,(method-lambda-list t)
                                  (declare (type com-wrapper ,this-var))
                                  (let ((,this-var
                                            (or (gethash ',interface-name
                                                         (%wrapper-interface-pointers ,this-var))
                                                (error 'com-error :code error-not-implemented))))
                                      (declare (type pointer ,this-var))
                                      ,(callout-form t)))
                                ',primary)))))))
          ,@(let ((trampoline-name (make-internal-name
                                     (package-name *package*)
                                     interface-name
                                     method-name
                                     'trampoline))
                  (warning (gensym "WARNING"))
                  (status (gensym (string 'status)))
                  (condition (gensym (string 'condition))))
             `((progn (define-callback (,trampoline-name :stdcall) ,return-type
                        ((,this-var pointer)
                         ,@(loop :for (arg-name arg-typespec . rest) :in args :collect
                             (list arg-name arg-typespec)))
                        ,(let* ((rtype (parse-typespec return-type))
                                (body `(let ((,rv-var ,(expand-prototype rtype)))
                                         (multiple-value-setq
                                           (,rv-var ,@(mapcar #'car args))
                                           ,(make-defmethod-call-form
                                                method-name this-var primary optional key))
                                         ,rv-var)))
                           (if (or (eq return-type 'hresult)
                                   (and (consp return-type)
                                        (eq (car return-type) 'hresult)))
                             `(let ((,status nil))
                                (declare (type (or null windows-status) ,status))
                                (handler-case
                                    (handler-bind
                                      ((windows-status
                                         (lambda (,warning)
                                           (when (null ,status)
                                             (setf ,status (make-condition 'windows-status)))
                                           (setf (windows-condition-code ,status)
                                                 (windows-condition-code ,warning))
                                           (muffle-warning ,warning))))
                                      (or ,body ,status))
                                  (windows-error (,condition) ,condition)
                                  (error (e)
                                         (format *error-output* "~&~a~%" e)
                                         (make-condition 'windows-error
                                           :code error-unexpected-failure))))
                             body)))
                      (setf (deref ,vtable-name 'pointer ,(* vtable-index (sizeof '*)))
                            (callback ,trampoline-name)))
               ,trampoline-name)))))))

(defun make-iid-association-form (name iid-name)
  (etypecase iid-name
    (guid `(setf (gethash (setf (slot-value (find-class ',name) '%iid) ,iid-name)
                          *iid-to-interface-class-mapping*)
                 (find-class ',name)))
    (string `(setf (gethash (setf (slot-value (find-class ',name) '%iid)
                                  (external-function-call
                                    "IIDFromString"
                                    ((:stdcall ole32)
                                     (hresult rv iid)
                                     ((& wstring))
                                     ((& guid :out) iid :aux))
                                    ,iid-name))
                            *iid-to-interface-class-mapping*)
                   (find-class ',name)))
    (symbol (assert (typep (symbol-value iid-name) 'guid) ())
            `(progn (setf (gethash ,iid-name *iid-to-interface-class-mapping*)
                          (find-class ',name))
                    (setf (slot-value (find-class ',name) '%iid) ,iid-name)))
    (cons (if (stringp (second iid-name))
            (progn
              (assert (and (symbolp (car iid-name))
                           (= 2 (length iid-name))))
              (let ((iid (external-function-call
                           "IIDFromString"
                           ((:stdcall ole32)
                            (hresult rv iid)
                            ((& wstring))
                            ((& guid :out) iid :aux))
                           (second iid-name))))
                (with-guid-accessors (dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8) iid                  
                  (make-iid-association-form
                      name (list (car iid-name) dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8)))))
            (destructuring-bind
                (iid-name dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8) iid-name
              `(progn (define-guid ,iid-name ,dw ,w1 ,w2
                        ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8)
                      (setf (gethash ,iid-name *iid-to-interface-class-mapping*)
                            (find-class ',name))
                      (setf (slot-value (find-class ',name) '%iid) ,iid-name)))))))

(defmacro define-interface (name-and-options
                             (&optional iid parent-name)
                             &rest doc-and-methods)
  (check-type parent-name symbol)
  (check-type name-and-options (or symbol cons))
  (check-type iid (or symbol cons string))
  (when (null iid)
    (setf iid (external-function-call
                "StringFromGUID2"
                ((:stdcall ole32)
                 ((last-error int doors::not-zero) rv buffer)
                 ((& guid))
                 ((& (~ wchar nil simple-string) :out)
                  buffer :aux (make-string 38 :initial-element #\space))
                 (int c :aux 39))
                (external-function-call
                  "CoCreateGuid"
                  ((:stdcall ole32)
                   (hresult rv guid)
                   ((& guid :out) guid :aux))))))
  (destructuring-bind
      (name &key (vtable-name (make-internal-name
                                (package-name *package*)
                                name
                                'vtable)))
      (ensure-list name-and-options)
    (check-type name symbol)
    (check-type vtable-name symbol)
    (let* ((doc (if (stringp (car doc-and-methods))
                  (car doc-and-methods)
                  nil))
           (methods (if (stringp (car doc-and-methods))
                      (rest doc-and-methods)
                      doc-and-methods))
           (parent (when parent-name
                     (find-interface-class parent-name)))
           (parent-methods (when parent
                             (com-interface-class-methods parent)))
           (direct-methods (mapcar (lambda (method-spec &aux (method-name (car method-spec)))
                                     (if (find method-name parent-methods :test #'equal)
                                       (error "You cannot override parent interface method: ~s"
                                              method-spec)
                                       method-name))
                             methods))
           (parent-vtable-name (when parent
                                 (com-interface-class-vtable-name parent))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(let ((form (make-iid-association-form name iid)))
              `((closer-mop:defclass ,name (,(or parent-name 'com-interface))
                  ()
                  (:metaclass com-interface-class)
                  (:vtable-name . ,vtable-name)
                  (:methods ,@parent-methods ,@direct-methods)
                  ,@(when doc `((:documentation ,doc))))
                (closer-mop:finalize-inheritance (find-class ',name))
                (loop :with this = (find-class ',name)
                  :with prev = nil
                  :for class :being :the :hash-values :of *iid-to-interface-class-mapping*
                  :using (:hash-key guid)
                  :when (eq prev this) :do (setf prev guid) (loop-finish)
                  :finally (when prev (remhash guid *iid-to-interface-class-mapping*)))
                ,form
                (pushnew (make-weak-pointer (find-class ',name))
                         *registered-interfaces*
                         :key #'weak-pointer-value
                         :test #'eq)
                (define-type-parser ,name (&optional add-ref)
                  (make-instance 'com-interface-type
                    :name ',name
                    :add-ref add-ref)))))
         (defvar ,vtable-name (raw-alloc ,(* (+ (length parent-methods)
                                                (length direct-methods))
                                             (sizeof 'pointer))))
         ,@(loop :for method-spec :in methods :collect
             `(define-interface-method ,name ,@method-spec))
         (define-struct (,vtable-name
                          ,@(when parent-vtable-name `((:include ,parent-vtable-name)))
                          (:constructor ,vtable-name))
             ,@(mapcar (lambda (x) `(,(intern (format nil "~s" x))
                                      pointer :initform (get-callback
                                                          ',(make-internal-name
                                                                (package-name *package*)
                                                              name
                                                              x
                                                              'trampoline))))
                 direct-methods))
         (setf (deref ,vtable-name ',vtable-name) (,vtable-name))
       ',name))))

(defmacro define-interface-method
    (interface-name method-name
     (return-type &optional (return-value-name (gensym))
                            (result-form return-value-name))
     &body doc-and-args)
  (check-type interface-name symbol)
  (check-type method-name (or symbol (cons (eql setf) (cons symbol null))))
  (check-type return-value-name symbol)
  (let* ((class (find-interface-class interface-name))
         (vtable-index (or (position method-name (com-interface-class-methods class) :test #'equal)
                           (error "Interface ~s has no method named ~s"
                                  interface-name method-name))))
    (destructuring-bind
        (method-def trampoline-def trampoline-name)
        (parse-method-spec interface-name
                           (com-interface-class-vtable-name class)
                           method-name
                           vtable-index
                           `((,return-type ,return-value-name ,result-form)
                             ,@doc-and-args))
      `(progn
         ,method-def
         ,trampoline-def
         ,@(labels ((frob (children)
                      (loop :for class :in children
                        :for child-vtable = (com-interface-class-vtable-name class)
                        :for child-methods = (com-interface-class-methods class)
                        :append (cons
                                  `(when (boundp ',child-vtable)
                                     (setf (deref (&+ ,child-vtable ,vtable-index '*) '*)
                                           (callback ,trampoline-name)))
                                  (ensure-list
                                    (frob (closer-mop:class-direct-subclasses class)))))))
             (frob (closer-mop:class-direct-subclasses class)))
         ',method-name))))
