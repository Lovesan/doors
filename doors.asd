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
;;; DEALINGS IN THE SOFTWARE.

(asdf:defsystem #:doors
  :version "0.4.3"
  :description "Doors, a lisper's gateway to Windows"
  :author "Dmitry Ignatiev <lovesan.ru@gmail.com>"
  :maintainer "Dmitry Ignatiev <lovesan.ru@gmail.com>"
  :licence "MIT"
  :depends-on (#:trivial-features #:alexandria #:virgil #:trivial-garbage #:closer-mop)
  :serial t
  :components ((:module "system"
                        :serial t
                        :components ((:file "package")
                                     (:file "libraries")
                                     (:file "features")
                                     (:file "wintypes")
                                     (:file "osversion")
                                     (:file "hresult")
                                     (:file "errors")
                                     (:file "guid")
                                     (:file "handles")
                                     (:file "dlls")
                                     (:file "time")
                                     (:file "sysinfo")
                                     ))
               (:module "com"
                        :serial t
                        :components ((:file "package")
                                     (:file "errors")
                                     (:file "interface")
                                     (:file "interface-defs")
                                     (:file "object")
                                     (:file "unknown")
                                     ))
               (:module "security"
                        :serial t
                        :components ((:file "package")
                                     (:file "authentication")
                                     (:file "authorization")
                                     ))
               (:module "system-aux"
                        :pathname "system"
                        :serial t
                        :components ((:file "console")
                                     (:file "registry")
                                     (:file "memory")
                                     (:file "processes")
                                     (:file "psapi")
                                     ))
               (:module "com-aux"
                        :pathname "com"
                        :serial t
                        :components ((:file "enumerations")
                                     (:file "structures")
                                     (:file "interfaces")
                                     (:file "functions")
                                     (:file "wrapper")))
                        ))

;; vim: ft=lisp et
