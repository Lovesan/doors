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

(define-external-function
    ("KillTimer" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Destroys the specified timer."
  (hwnd handle)
  (id uint-ptr))

(define-external-function
    ("SetTimer" (:camel-case))
    (:stdcall user32)
  (uint-ptr)
  "Creates a timer with the specified time-out value."
  (hwnd handle :optional)
  (id uint-ptr :optional)
  (elapse uint)
  (callback pointer :optional))

(define-external-function
    (#+doors.unicode "EnumPropsW"
     #-doors.unicode "EnumPropsA"
                   enum-props)
    (:stdcall user32)
  (int rv (if (= -1 rv) nil rv))
  "Enumerates all entries in the property list of a window by passing them, one by one, to the specified callback function."
  (hwnd handle)
  (func pointer))

(define-external-function
    (#+doors.unicode "EnumPropsExW"
     #-doors.unicode "EnumPropsExA"
                   enum-props*)
    (:stdcall user32)
  (int rv (if (= -1 rv) nil rv))
  "Enumerates all entries in the property list of a window by passing them, one by one, to the specified callback function."
  (hwnd handle)
  (func pointer)
  (lparam lparam))

(define-external-function
    (#+doors.unicode "GetPropW"
     #-doors.unicode "GetPropA"
                   window-prop)
    (:stdcall user32)
  (handle)
  "Retrieves a data handle from the property list of the specified window."
  (hwnd handle)
  (id (union ()
        (string (& tstring))
        (atom uint-ptr))))

(define-external-function
    (#+doors.unicode "RemovePropW"
     #-doors.unicode "RemovePropA"
                   remove-prop)
    (:stdcall user32)
  (handle)
  "Removes an entry from the property list of the specified window."
  (hwnd handle)
  (id (union ()
        (string (& tstring))
        (atom uint-ptr))))

(define-external-function
    (#+doors.unicode "SetPropW"
     #-doors.unicode "SetPropA"
                   (setf window-prop))
    (:stdcall user32)
  ((last-error bool) rv data)
  "Adds a new entry or changes an existing entry in the property list of the specified window."
  (hwnd handle :optional)
  (id (union ()
        (string (& tstring))
        (atom uint-ptr))
      :optional 0)
  (data handle))

(define-struct (cbt-create-wnd)
    "Contains information passed to a WH-CBT hook procedure, before a window is created."
  (cs (& create-struct))
  (insert-after handle))

(define-struct (cbt-activate-struct)
    "Contains information passed to a WH-CBT hook procedure, before a window is activated."
  (mouse boolean)
  (active-hwnd handle))

(define-struct (cwp-ret-struct)
    "Defines the message parameters passed to a WH-CALL-WNDPROC-RET hook procedur"
  (lresult lresult)
  (lparam lparam)
  (wparam wparam)
  (message uint)
  (hwnd handle))

(define-struct (cwp-struct)
    "Defines the message parameters passed to a WH-CALL-WNDPROC hook procedure"
  (lparam lparam)
  (wparam wparam)
  (message uint)
  (hwnd handle))

(define-struct (debug-hook-info
                 (:constructor make-dh-info)
                 (:conc-name dh-info-))
    "Contains debugging information passed to a WH-DEBUG hook procedure"
  (thread-id dword)
  (installer-thread-id dword)
  (lparam lparam)
  (wparam wparam)
  (code int))

(define-struct (event-msg)
    "Contains information about a hardware message sent to the system message queue."
  (message uint)
  (param-l uint)
  (param-h uint)
  (time dword)
  (hwnd handle))

(define-struct (kdb-ll-hook-struct
                 (:constructor make-kdb-ll-hs)
                 (:conc-name kdb-ll-hs-))
    "Contains information about a low-level keyboard input event."
  (code dword)
  (scan-code dword)
  (flags (enum (:base-type dword :list t)
           (:extended #b1)
           (:injected #b10000)
           (:alt-down #b100000)
           (:up #b10000000)))
  (time dword)
  (extra-info ulong-ptr))

(define-struct (mouse-hook-struct
                 (:conc-name mhs-)
                 (:constructor make-mhs))
    "Contains information about a mouse event passed to a WH-MOUSE hook procedure"
  (pt point)
  (hwnd handle)
  (hit-test-code hit-test-code)
  (extra-info ulong-ptr))

(define-struct (mouse-hook-struct*
                 (:include mouse-hook-struct)
                 (:conc-name mhs-)
                 (:constructor make-mhs*))
    "Contains information about a mouse event passed to a WH-MOUSE hook procedure"
  (mouse-data dword))

(define-struct (ms-ll-hook-struct
                 (:conc-name ms-ll-hs-)
                 (:constructor make-ms-ll-hs))
    "Contains information about a low-level mouse input event."
  (pt point)
  (mouse-data dword)
  (flags (enum (:base-type uint)
           (:injected 1)))
  (time dword)
  (extra-info ulong-ptr))

(define-external-function
    (#+doors.unicode "CallMsgFilterW"
     #-doors.unicode "CallMsgFilterA"
                   call-msg-filter)
    (:stdcall user32)
  (bool)
  "Passes the specified message and hook code to the hook procedures associated with the WH-SYS-MSG-FILTER and WH-MSG-FILTER hooks."
  (msg (& msg))
  (code int))

(define-enum (hook-type
               (:base-type int)
               (:conc-name wh-))
  (:call-wndproc 4)
  (:call-wndproc-ret 12)
  (:cbt 5)
  (:debug 9)
  (:foreground-idle 11)
  (:get-message 3)
  (:journal-playback 1)
  (:journal-record 0)
  (:keyboard 2)
  (:keyboard-ll 13)
  (:mouse 7)
  (:mouse-ll 14)
  (:msg-filter -1)
  (:shell 10)
  (:sys-msg-filter 6))

(define-external-function
    ("CallNextHookEx" call-next-hook)
    (:stdcall user32)
  (lresult)
  "Passes the hook information to the next hook procedure in the current hook chain."
  (hhk handle :aux)
  (type hook-type)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    (#+doors.unicode "SetWindowsHookExW"
     #-doors.unicode "SetWindowsHookExA"
                   set-windows-hook)
    (:stdcall user32)
  ((last-error handle))
  "Installs an application-defined hook procedure into a hook chain."
  (type hook-type)
  (callback pointer)
  (module handle :optional)
  (thread-id dword :optional))

(define-external-function
    ("UnhookWindowsHookEx" unhook-windows-hook)
    (:stdcall user32)
  ((last-error bool))
  "Removes a hook procedure installed in a hook chain by the set-windows-hook function."
  (hook handle))

(define-struct (mdi-create-struct
                 (:conc-name mdi-cs-)
                 (:constructor make-mdi-cs))
    "Contains information about the class, title, owner, location, and size of a multiple-document interface (MDI) child window."
  (class (union ()
           (string (& (const tstring)))
           (atom uint-ptr)))
  (title (& (const tstring)))
  (owner handle)
  (x (enum (:base-type int)
       (:use-default #.(make-long 0 #x8000))))
  (y (enum (:base-type int)
       (:use-default #.(make-long 0 #x8000))))
  (cx (enum (:base-type int)
        (:use-default #.(make-long 0 #x8000))))
  (cy (enum (:base-type int)
        (:use-default #.(make-long 0 #x8000))))
  (style window-style)
  (lparam lparam))

(define-external-function
    (#+doors.unicode "CreateMDIWindowW"
     #-doors.unicode "CreateMDIWindowA"
                   create-mdi-window)
    (:stdcall user32)
  ((last-error handle))
  "Creates a multiple-document interface (MDI) child window."
  (class-name (union ()
                (string (& (const tstring)))
                (atom uint-ptr))
              :key 0)
  (window-name (& tstring) :key)
  (style window-style :key)
  (x (enum (:base-type int)
       (:use-default #.(make-long 0 #x8000))) :key :use-default)
  (y (enum (:base-type int)
       (:use-default #.(make-long 0 #x8000))) :key :use-default)
  (width (enum (:base-type int)
           (:use-default #.(make-long 0 #x8000))) :key :use-default)
  (height (enum (:base-type int)
            (:use-default #.(make-long 0 #x8000))) :key :use-default)
  (parent handle :key)
  (instance handle :key)
  (param pointer :key))

(define-external-function
    (#+doors.unicode "DefFrameProcW"
     #-doors.unicode "DefFrameProcA"
                   def-frame-proc)
    (:stdcall user32)
  (lresult)
  "Provides default processing for any window messages that the window procedure of a multiple-document interface (MDI) frame window does not process."
  (frame handle)
  (mdi-client handle)
  (msg uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    (#+doors.unicode "DefMDIChildProcW"
     #-doors.unicode "DefMDIChildProcA"
                   def-mdi-child-proc)
    (:stdcall user32)
  (lresult)
  "Provides default processing for any window message that the window procedure of a multiple-document interface (MDI) child window does not process."
  (hwnd handle)
  (msg uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    ("TranslateMDISysAccel" translate-mdi-sys-accel)
    (:stdcall user32)
  (bool)
  "Processes accelerator keystrokes for window menu commands of the multiple-document interface (MDI) child windows associated with the specified MDI client window."
  (client handle)
  (msg (& msg)))

