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
  `(,method-name
     ,@(if (consp method-name)
         `(,(car primary)
             (gethash ,this-var *pointer-to-object-mapping*)
             ,@(rest primary))
         `((gethash ,this-var *pointer-to-object-mapping*)
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
    (check-type method-name symbol)
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
        `((defmethod ,method-name (,@(if (consp method-name)
                                       `(,(car primary)
                                           (,this-var ,interface-name)
                                           ,@(rest primary))
                                       `((,this-var ,interface-name)
                                         ,@primary))
                                      ,@(unless (null key)
                                          (make-method-arg-group types key '&key))
                                      ,@(unless (null optional)
                                          (make-method-arg-group types optional '&optional))
                                      ,@(unless (null aux)
                                          (make-method-arg-group types aux '&aux)))
            (declare (type ,interface-name ,this-var)
                     ,@(loop :for (name lisp-type initform) :in types
                        :collect `(type ,lisp-type ,name)))
            ,@(ensure-list doc)
            (external-pointer-call
              (deref (&+ (deref (com-interface-pointer ,this-var) 'pointer)
                      ,vtable-index
                      'pointer)
                     'pointer)
              ((:stdcall)
               (,return-type ,return-value-name ,result-form)
               (,interface-name ,this-var :aux ,this-var)
               ,@normalized)))
          ,@(let ((trampoline-name (make-internal-name
                                     (package-name *package*)
                                     interface-name
                                     method-name
                                     'trampoline))
                  (condition (gensym (string 'condition))))
             `((progn (define-callback (,trampoline-name :stdcall) ,return-type
                        ((,this-var size-t)
                         ,@(loop :for (arg-name arg-typespec . rest) :in args :collect
                             (list arg-name arg-typespec)))
                        ,(let ((body `(let ((,rv-var ,(expand-prototype (parse-typespec return-type))))
                                        (multiple-value-setq
                                          (,rv-var ,@(mapcar #'car args))
                                          ,(make-defmethod-call-form
                                              method-name this-var primary optional key))
                                        ,rv-var)))
                           (if (eq return-type 'hresult)
                             `(handler-case
                                  ,body
                                (windows-error (,condition) ,condition)
                                (error () (make-condition 'windows-error
                                             :code error-unexpected-failure)))
                             body)))
                      (setf (deref ,vtable-name 'pointer ,(* vtable-index (sizeof '*)))
                            (callback ,trampoline-name)))
               ,trampoline-name)))))))

(defun make-iid-association-form (name iid-name)
  (etypecase iid-name
    (null nil nil)
    (symbol (assert (typep (symbol-value iid-name) 'iid) ())
            (values `(eval-when (:compile-toplevel :load-toplevel :execute)
                       (setf (gethash ,iid-name *iid-to-interface-class-mapping*)
                             (find-class ',name)))
                    iid-name))
    (cons (destructuring-bind
              (iid-name dw w1 w2 b1 b2 b3 b4 b5 b6 b7 b8) iid-name
            (values `(eval-when (:compile-toplevel :load-toplevel :execute)
                       (define-iid ,iid-name ,dw ,w1 ,w2
                         ,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8)
                       (setf (gethash ,iid-name *iid-to-interface-class-mapping*)
                             (find-class ',name)))
                    iid-name)))))

(defmacro define-interface (name-and-options
                             (&optional iid parent-name)
                             &rest doc-and-methods)
  (check-type parent-name symbol)
  (check-type name-and-options (or symbol cons))
  (check-type iid (or symbol cons))
  (destructuring-bind
      (name &key (vtable-name (intern (format nil "~a-~a" name 'vtable))))
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
                                 (com-interface-class-vtable-name parent)))
           (vtable-size (+ (length parent-methods) (length direct-methods))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(multiple-value-bind
                (form iid-name) (make-iid-association-form name iid)
              `((closer-mop:defclass ,name (,(or parent-name 'com-interface))
                  ()
                  (:metaclass com-interface-class)
                  (:vtable-name . ,vtable-name)
                  (:methods ,@parent-methods ,@direct-methods))
                (closer-mop:finalize-inheritance (find-class ',name))
                ,form
                (setf (slot-value (find-class ',name) 'iid) ,iid-name)
                (define-type-parser ,name (&optional finalize)
                  (make-instance 'com-interface-type
                    :name ',name
                    :finalize finalize)))))
         ,@(let ((methods (append parent-methods direct-methods)))
            `((define-struct (,vtable-name (:constructor ,vtable-name ,methods))
                 ,@(mapcar (lambda (x) (list x 'pointer)) methods))
              (defvar ,vtable-name (alloc ',vtable-name))
              ,@(when parent
                 `((setf (deref ,vtable-name
                                ',parent-vtable-name)
                         (deref ,parent-vtable-name
                                ',parent-vtable-name))))))
       ,@(loop :for vtable-index :from (length parent-methods)
           :for method-spec :in methods :collect
           `(define-interface-method ,name ,@method-spec))
       ',name))))

(defmacro define-interface-method
    (interface-name method-name
     (return-type &optional (return-value-name (gensym))
                            (result-form return-value-name))
     &body doc-and-args)
  (check-type interface-name symbol)
  (check-type method-name symbol)
  (check-type return-value-name symbol)
  (let* ((class (find-interface-class interface-name))
         (vtable-index (or (position method-name (com-interface-class-methods class))
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
                                  `(setf (deref (&+ ,child-vtable ,vtable-index '*) '*)
                                         (callback ,trampoline-name))
                                  (ensure-list
                                    (frob (closer-mop:class-direct-subclasses class)))))))
             (frob (closer-mop:class-direct-subclasses class)))
         ',method-name))))
