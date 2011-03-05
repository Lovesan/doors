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

(in-package #:doors.ui)

(define-enum (class-style
               (:base-type dword)
               (:conc-name cs-)
               (:list t))
  (:byte-align-center #x1000)
  (:byte-align-window #x2000)
  (:class-dc #x0040)
  (:dbl-clks #x0008)
  (:drop-shadow #x00020000)
  (:global-class #x4000)
  (:hredraw #x0002)
  (:no-close #x0200)
  (:own-dc #x0020)
  (:parent-dc #x0080)
  (:save-bits #x0800)
  (:vredraw #x0001))

(define-struct (wndclass
                 (:conc-name wndclass-)
                 (:constructor make-wndclass
                               (&key style wndproc cls-extra
                                     wnd-extra instance icon cursor
                                     background menu-name class-name
                                     small-icon)))
    "Contains the window class attributes that are registered by the register-class and class-info functions."
  (cb-size dword :initform (sizeof 'wndclass))
  (style class-style)
  (wndproc pointer)
  (cls-extra int)
  (wnd-extra int)
  (instance handle)
  (icon handle)
  (cursor handle)
  (background (union ()
                 (handle handle)
                 (uint uint-ptr))
               :initform &0)
  (menu-name (union ()
                (string (& (const tstring)))
                (uint uint-ptr))
              :initform 0)
  (class-name (union ()
                 (string (& (const tstring)))
                 (uint uint-ptr))
               :initform 0)
  (small-icon handle))

(define-external-function
    (#+doors.unicode "GetClassInfoExW"
     #-doors.unicode "GetClassInfoExA"
                   class-info)
    (:stdcall user32)
  ((last-error bool) rv info)
  "Retrieves information about a window class, including a handle to the small icon associated with the window class."
  (instance handle :optional)
  (class-name (union ()
               (string (& (const tstring)))
               (uint uint-ptr)))
  (info (& wndclass :inout) :aux))

(define-external-function
    (#+doors.unicode "GetClassLongW"
     #-doors.unicode "GetClassLongA"
                   class-long)
    (:stdcall user32)
  ((last-error dword doors::not-zero))
  "Retrieves the specified 32-bit (DWORD) value from the WNDCLASS structure associated with the specified window."
  (hwnd handle)
  (index (enum (:base-type int)
           (:atom -32)
           (:cb-cls-extra -20)
           (:cb-wnd-extra -18)
           (:background -10)
           (:cursor -12)
           (:icon -14)
           (:small-icon -34)
           (:module -16)
           (:menu-name -8)
           (:style -26)
           (:wndproc -24))))

(define-external-function
    (#+doors.unicode #+x86-64 "GetClassLongPtrW" #-x86-64 "GetClassLongW"
     #-doors.unicode #+x86-64 "GetClassLongPtrA" #-x86-64 "GetClassLongA"
                   class-long-ptr)
    (:stdcall user32)
  ((last-error dword doors::not-zero))
  "Retrieves the specified value from the WNDCLASS structure associated with the specified window."
  (hwnd handle)
  (index (enum (:base-type int)
           (:atom -32)
           (:cb-cls-extra -20)
           (:cb-wnd-extra -18)
           (:background -10)
           (:cursor -12)
           (:icon -14)
           (:small-icon -34)
           (:module -16)
           (:menu-name -8)
           (:style -26)
           (:wndproc -24))))

(define-external-function
    (#+doors.unicode "GetClassNameW"
     #-doors.unicode "GetClassNameA"
                   window-class-name)
    (:stdcall user32)
  ((last-error int doors::not-zero) rv
   (subseq buffer 0 rv))
  "Retrieves the name of the class to which the specified window belongs."
  (hwnd handle)
  (buffer (& tstring :out) :aux (make-string count))
  (count int :optional 256))

(define-external-function
    ("GetClassWord" class-word)
    (:stdcall user32)
  ((last-error word doors::not-zero))
  "Retrieves the 16-bit (WORD) value at the specified offset into the extra class memory for the window class to which the specified window belongs."
  (hwnd handle)
  (index (enum (:base-type int)
           (:atom -32))))

(define-external-function
    (#+doors.unicode "GetWindowLongW"
     #-doors.unicode "GetWindowLongA"
                   window-long)
    (:stdcall user32)
  ((last-error long doors::not-zero))
  "Retrieves information about the specified window. The function also retrieves the 32-bit (DWORD) value at the specified offset into the extra window memory."
  (hwnd handle)
  (index (enum (:base-type int)
           (:style* -20)
           (:instance -6)
           (:parent -8)
           (:id -12)
           (:style -16)
           (:user-data -21)
           (:wndproc -4))))

(define-external-function
    (#+doors.unicode #+x86-64 "GetWindowLongPtrW" #-x86-64 "GetWindowLongW"
     #-doors.unicode #+x86-64 "GetWindowLongPtrA" #-x86-64 "GetWindowLongA"
                   window-long-ptr)
    (:stdcall user32)
  ((last-error long-ptr doors::not-zero))
  "Retrieves information about the specified window. The function also retrieves the value at a specified offset into the extra window memory."
  (hwnd handle)
  (index (enum (:base-type int)
           (:style* -20)
           (:instance -6)
           (:parent -8)
           (:id -12)
           (:style -16)
           (:user-data -21)
           (:wndproc -4))))

(define-external-function
    (#+doors.unicode "RegisterClassExW"
     #-doors.unicode "RegisterClassExA"
                   register-class)
    (:stdcall user32)
  ((last-error atom doors::not-zero))
  "Registers a window class for subsequent use in calls to the create-window or create-window* function."
  (window-class (& wndclass)))

(define-external-function
    (#+doors.unicode "SetClassLongW"
     #-doors.unicode "SetClassLongA"
                   (setf class-long))
    (:stdcall user32)
  ((last-error dword doors::not-zero))
  "Replaces the specified 32-bit (long) value at the specified offset into the extra class memory or the WNDCLASS structure for the class to which the specified window belongs."
  (hwnd handle :optional)
  (index (enum (:base-type int)
           (:cb-cls-extra -20)
           (:cb-wnd-extra -18)
           (:background -10)
           (:cursor -12)
           (:icon -14)
           (:small-icon -34)
           (:module -16)
           (:menu-name -8)
           (:style -26)
           (:wndproc -24))
         :optional)
  (new-long long))

(define-external-function
    (#+doors.unicode #+x86-64 "SetClassLongPtrW" #-x86-64 "SetClassLongW"
     #-doors.unicode #+x86-64 "SetClassLongPtrA" #-x86-64 "SetClassLongA"
                   (setf class-long-ptr))
    (:stdcall user32)
  ((last-error long-ptr doors::not-zero))
  "Replaces the specified value at the specified offset into the extra class memory or the WNDCLASS structure for the class to which the specified window belongs."
  (hwnd handle :optional)
  (index (enum (:base-type int)
           (:cb-cls-extra -20)
           (:cb-wnd-extra -18)
           (:background -10)
           (:cursor -12)
           (:icon -14)
           (:small-icon -34)
           (:module -16)
           (:menu-name -8)
           (:style -26)
           (:wndproc -24))
         :optional)
  (new-long-ptr long-ptr))

(define-external-function
    ("SetClassWord" (setf class-word))
    (:stdcall user32)
  ((last-error word doors::not-zero))
  "Replaces the 16-bit (WORD) value at the specified offset into the extra class memory for the window class to which the specified window belongs."
  (hwnd handle :optional)
  (index int :optional)
  (new-word word))

(define-external-function
    (#+doors.unicode "SetWindowLongW"
     #-doors.unicode "SetWindowLongA"
                   (setf window-long))
    (:stdcall user32)
  ((last-error long doors::not-zero))
  "Changes an attribute of the specified window. The function also sets the 32-bit (long) value at the specified offset into the extra window memory."
  (hwnd handle :optional)
  (index (enum (:base-type int)
           (:style* -20)
           (:instance -6)
           (:id -12)
           (:style -16)
           (:user-data -21)
           (:wndproc -4))
         :optional)
  (new-long long))

(define-external-function
    (#+doors.unicode #+x86-64 "SetWindowLongPtrW" #-x86-64 "SetWindowLongW"
     #-doors.unicode #+x86-64 "SetWindowLongPtrA" #-x86-64 "SetWindowLongA"
                   (setf window-long-ptr))
    (:stdcall user32)
  ((last-error long-ptr doors::not-zero))
  "Changes an attribute of the specified window. The function also sets a value at the specified offset in the extra window memory."
  (hwnd handle :optional)
  (index (enum (:base-type int)
           (:style* -20)
           (:instance -6)
           (:id -12)
           (:style -16)
           (:user-data -21)
           (:wndproc -4))
         :optional)
  (new-long-ptr long-ptr))

(define-external-function
    (#+doors.unicode "UnregisterClassW"
     #-doors.unicode "UnregisterClassA"
                   unregister-class)
    (:stdcall user32)
  ((last-error bool))
  "Unregisters a window class, freeing the memory required for the class."
  (class-name (union ()
                (string (& (const tstring)))
                (uint uint-ptr)))
  (instance handle :optional))

(define-enum (system-color
               (:base-type dword)
               (:conc-name color-))
  (:3d-dk-shadow 21)
  (:3d-face 15)
  (:3d-highlight 20)
  (:3d-hilight 20)
  (:3d-light 22)
  (:3d-shadow 16)
  (:active-border 10)
  (:active-caption 2)
  (:app-workspace 12)
  (:background 1)
  (:btn-face 15)
  (:btn-highlight 20)
  (:btn-hilight 20)
  (:btn-shadow 16)
  (:btn-text 18)
  (:caption-text 9)
  (:desktop 1)
  (:gradient-active-caption 27)
  (:gradient-inactive-caption 28)
  (:gray-text 17)
  (:highlight 13)
  (:highlight-text 14)
  (:hotlight 26)
  (:inactive-border 11)
  (:inactive-caption 3)
  (:inactive-caption-text 19)
  (:info-bk 24)
  (:info-text 23)
  (:menu 4)
  (:menu-hilight 29)
  (:menu-bar 30)
  (:menu-text 7)
  (:scrollbar 0)
  (:window 5)
  (:window-frame 6)
  (:window-text 8))
