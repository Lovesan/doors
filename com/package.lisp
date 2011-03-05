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

(in-package #:cl-user)

(defpackage #:doors.com
  (:use #:cl #:alexandria #:trivial-garbage #:virgil #:doors)
  (:nicknames #:com)
  (:export
   
    #:iid
    #:clsid
    
    ;;errors
    #:com-error
    #:com-error-code
    
    #:error-not-implemented
    #:error-no-interface
    #:error-invalid-pointer
    #:error-abort
    #:error-failure
    #:error-data-pending
    #:error-class-not-available
    #:error-class-not-registered
    
    #:error-unexpected-failure
    #:error-not-implemented
    
    ;;com-interface related stuff
    #:com-interface-class
    #:com-interface
    #:com-generic-function
    #:find-interface-class
    #:com-interface-pointer
    #:com-interface-method-pointer
    #:translate-interface
    #:convert-interface
    #:define-interface
    #:define-interface-method
    
    ;;com-object related stuff
    #:com-class
    #:com-object
    #:find-com-class
    #:acquire-interface
    #:deinitialize-vtables
    #:reinitialize-vtables
    
    ;;IUnknown
    #:iid-unknown
    #:unknown
    #:query-interface    
    #:add-ref
    #:release
    
    #:with-interface
    #:with-interfaces
    
    ;;enumerations
    #:com-rights
    #:com-rights-execute
    #:com-rights-execute-local
    #:com-rights-execute-remote
    #:com-rights-activate-local
    #:com-rights-activate-remote
    #:class-object-registration-flags
    #:regcls-single-use
    #:regcls-miltiple-use
    #:regcls-multi-separate
    #:regcls-suspended
    #:regcls-surrogate
    #:class-context-flags
    #:clsctx-inproc-server
    #:clsctx-inproc-handler
    #:clsctx-inproc
    #:clsctx-local-server
    #:clsctx-remote-server
    #:clsctx-no-code-download
    #:clsctx-no-custom-marshal
    #:clsctx-enable-code-download
    #:clsctx-no-failure-log
    #:clsctx-disable-aaa
    #:clsctx-enable-aaa
    #:clsctx-form-default-context
    #:clsctx-activate-32-bit-server
    #:clsctx-activate-64-bit-server
    #:clsctx-enable-cloaking
    #:clsctx-all
    #:clsctx-server
    #:clsctx-ps-dll
    #:com-init-flags
    #:coinit-multithreaded
    #:coinit-apartament-threaded
    #:coinit-disable-ole1-dde
    #:coinit-speed-over-memory
    #:bind-flags
    #:bind-may-bother-user
    #:bind-just-test-existence
    #:stgm-flags
    #:stgm-read
    #:stgm-write
    #:stgm-read-wrie
    #:stgm-share-deny-none
    #:stgm-share-deny-read
    #:stgm-share-deny-write
    #:stgm-share-exclusive
    #:stgm-priority
    #:stgm-create
    #:stgm-convert
    #:stgm-fail-if-there
    #:stgm-direct
    #:stgm-transacted
    #:stgm-no-scratch
    #:stgm-no-snapshot
    #:stgm-simple
    #:stgm-direct-swmr
    #:stgm-delete-on-release
    
    ;;structures
    #:server-info
    #:make-server-info
    #:server-info-name
    #:server-info-auth-info
    #:auth-info
    #:make-auth-info
    #:auth-info-authn-svc
    #:auth-info-authz-svc
    #:auth-info-server-principal-name
    #:auth-info-auth-level
    #:auth-info-impersonation-level
    #:auth-info-auth-identity-data
    #:auth-info-capabilities
    #:bind-options
    #:make-bind-options
    #:bind-opt-flags
    #:bind-opt-mode
    #:bind-opt-tick-count-deadline
    #:multi-qi
    #:make-multi-qi
    #:multi-qi-iid
    #:multi-qi-interface
    #:multi-qi-hresult
    
    ;;interfaces
    #:class-factory
    #:iid-class-factory
    #:create-instance
    #:lock-server
    
    ;;functions
    #:register-class-object
    #:revoke-class-object
    #:class-object
    #:create-com-instance
    #:initialize-com
    #:uninitialize-com
    #:create-guid
    #:task-mem-alloc
    #:task-mem-realloc
    #:task-mem-free
    #:string-from-iid
    #:iid-from-string
    #:initialize-com*
    #:clsid-from-progid
    #:clsid-from-progid*
    #:clsid-from-string
    #:string-from-clsid
    #:progid-from-clsid
    #:string-from-guid
    #:create-com-instance*
    #:add-ref-server-process
    #:release-server-process
    
    ;;wrapper stuff
    #:com-wrapper-class
    #:com-wrapper
    #:com-wrapper-context
    #:com-wrapper-server-info
    ))
