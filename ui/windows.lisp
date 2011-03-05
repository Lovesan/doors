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

(define-enum (window-style
               (:conc-name ws-)
               (:list t)
               (:base-type dword))
  (:border #x00800000)
  (:caption #x00C00000)
  (:child #x40000000)
  (:child-window #x40000000)
  (:clip-children #x02000000)
  (:clip-siblings #x04000000)
  (:disabled #x08000000)
  (:dlg-frame #x00400000)
  (:group #x00020000)
  (:hscroll #x00100000)
  (:iconic #x20000000)
  (:maximize #x10000000)
  (:maximize-box #x00010000)
  (:minimize #x20000000)
  (:minimize-box #x00020000)
  (:overlapped #x00000000)
  (:overlapped-window #x00CF0000)
  (:popup #x80000000)
  (:popup-window #x80880000)
  (:size-box #x00040000)
  (:sys-menu #x00080000)
  (:tab-stop #x00010000)
  (:thick-frame #x00040000)
  (:tiled #x00000000)
  (:tiled-window #x00CF0000)
  (:visible #x10000000)
  (:vscroll #x00200000))

(define-enum (window-style*
               (:conc-name ws-ex-)
               (:list t)
               (:base-type dword))
  (:accept-files #x00000010)
  (:app-window #x00040000)
  (:client-edge #x00000200)
  (:composited #x02000000)
  (:context-help #x00000400)
  (:control-parent #x00010000)
  (:dlg-modal-frame #x00000001)
  (:layered #x00080000)
  (:layout-rtl #x00400000)
  (:left #x00000000)
  (:left-scrollbar #x00004000)
  (:ltr-reading #x00000000)
  (:mdi-child #x00000040)
  (:no-activate #x08000000)
  (:no-inherit-layout #x00100000)
  (:no-parent-notify #x00000004)
  (:overlapped-window #x00000300)
  (:palette-window #x00000088)
  (:right #x00001000)
  (:right-scrollbar #x00000000)
  (:rtl-reading #x00002000)
  (:static-edge #x00020000)
  (:tool-window #x00000080)
  (:topmost #x00000008)
  (:transparent #x00000020)
  (:window-edge #x00000100))

(define-enum (window-message
               (:base-type dword)
               (:conc-name wm-))
  (:get-hmenu #x01E1)
  (:erase-background #x0014)
  (:get-font #x0031)
  (:get-text #x000D)
  (:get-text-length #x000E)
  (:set-font #x0030)
  (:set-icon #x0080)
  (:set-text #x000C)
  (:user #x0400)
  (:app #x8000)
  (:mdi-activate #x0222)
  (:mdi-cascade #x0227)
  (:mdi-create #x0220)
  (:mdi-destroy #x0221)
  (:mdi-get-active #x0222)
  (:mdi-icon-arrange #x0228)
  (:mdi-maximize #x0225)
  (:mdi-next #x0224)
  (:mdi-refresh-menu #x0234)
  (:mdi-restore #x0223)
  (:mdi-set-menu #x0230)
  (:mdi-tile #x0226)
  (:winini-change #x001A)
  (:setting-change #x001A))

(defconstant ocm-base (+ wm-user #x1c00))

(define-enum (window-notifications
               (:conc-name wm-)
               (:base-type dword))
  (:activate-app #x001C)
  (:cancel-journal #x004B)
  (:cancel-mode #x001F)
  (:child-activate #x0022)
  (:close #x0010)
  (:compacting #x0041)
  (:create #x0001)
  (:destroy #x0002)
  (:enable #x000A)
  (:enter-size-move #x0231)
  (:exit-size-move #x0232)
  (:get-icon #x007F)
  (:get-minmax-info #x0024)
  (:input-lang-change #x0051)
  (:input-lang-change-request #x0050)
  (:move #x0003)
  (:moving #x0216)
  (:nc-activate #x0086)
  (:nc-calc-size #x0083)
  (:nc-create #x0081)
  (:nc-destroy #x0082)
  (:null #x0000)
  (:parent-notify #x0210)
  (:query-drag-icon #x0037)
  (:query-open #x0013)  
  (:quit #x0012)
  (:queue-sync #x0023)
  (:show-window #x0018)
  (:size #x0005)
  (:sizing #x0214)
  (:style-changed #x007D)
  (:style-changing #x007C)
  (:timer #x0113)
  (:theme-changed #x031A)
  (:window-pos-changed #x0047)
  (:window-pos-changing #x0046))

(define-struct (alt-tab-info
                 (:constructor make-alt-tab-info
                               (&key items columns rows
                                col-focus row-focus
                                cx-item cy-item start)))
    "Contains status information for the application-switching (ALT+TAB) window."
  (cb dword :initform (sizeof 'alt-tab-info))
  (items int)
  (columns int)
  (rows int)
  (col-focus int)
  (row-focus int)
  (cx-item int)
  (cy-item int)
  (start point))

(define-struct (change-filter-struct
                 (:conc-name cfs-)
                 (:constructor make-change-filter-struct
                               (&optional exit-status)))
    "Contains extended result information obtained by calling the change-window-message-filter* function."
  (cb dword :initform (sizeof 'change-filter-struct))
  (exit-status (enum (:base-type dword)
                 (:none 0)
                 (:allowed-higher 3)
                 (:already-allowed 1)
                 (:already-disallowed 2))))

(define-struct (client-create-struct
                 (:conc-name client-cs-))
    "Contains information about the menu and first multiple-document interface (MDI) child window of an MDI client window."
  (window-menu handle)
  (first-child-id uint))

(define-struct (create-struct
                 (:conc-name cs-))
    "Defines the initialization parameters passed to the window procedure of an application."
  (create-params pointer)
  (instance handle)
  (menu handle)
  (parent handle)
  (cy (enum (:base-type int)
        (:use-default #.(make-long 0 #x8000))))
  (cx (enum (:base-type int)
        (:use-default #.(make-long 0 #x8000))))
  (y (enum (:base-type int)
       (:use-default #.(make-long 0 #x8000))))
  (x (enum (:base-type int)
       (:use-default #.(make-long 0 #x8000))))
  (style window-style)
  (name (& (const tstring)))
  (class (union ()
           (string (& (const tstring)))
           (uint uint-ptr))
         :initform 0)
  (style* window-style*))

(define-struct (gui-thread-info
                 (:conc-name gti-)
                 (:constructor make-gui-thread-info
                               (&key flags active focus capture menu-owner
                                move-size caret caret-rect)))
    "Contains information about a GUI thread."
  (cb dword :initform (sizeof 'gui-thread-info))
  (flags (enum (:base-type dword :list t)
           (:caret-blinking #x01)
           (:in-menu-mode #x04)
           (:in-move-size #x02)
           (:popup-menu-mode #x10)
           (:system-menu-mode #x08)))
  (active handle)
  (focus handle)
  (capture handle)
  (menu-owner handle)
  (move-size handle)
  (caret handle)
  (caret-rect rect))

(define-struct (minmax-info
                 (:constructor make-minmax-info
                               (&key max-size max-position
                                min-track-size max-track-size)))
    "Contains information about a window's maximized size and position and its minimum and maximum tracking size."
  (reserved point)  
  (max-size point)
  (max-position point)
  (min-track-size point)
  (max-track-size point))

(define-struct (window-pos
                 (:conc-name wpos-))
    "Contains information about the size and position of a window."
  (hwnd handle)
  (hwnd-insert-after handle)
  (x (enum (:base-type int)
       (:use-default #.(make-long 0 #x8000))))
  (y (enum (:base-type int)
       (:use-default #.(make-long 0 #x8000))))
  (cx (enum (:base-type int)
        (:use-default #.(make-long 0 #x8000))))
  (cy (enum (:base-type int)
        (:use-default #.(make-long 0 #x8000))))
  (flags (enum (:base-type uint :list t)
           (:draw-frame #x0020)
           (:frame-changed #x0020)
           (:hide-window #x0080)
           (:no-activate #x0010)
           (:no-copy-bits #x0100)
           (:no-move #x0002)
           (:no-owner-z-order #x0200)
           (:no-redraw #x0008)
           (:no-reposition #x0200)
           (:no-send-changing #x0400)
           (:no-size #x0001)
           (:no-z-order #x0004)
           (:show-window #x0040))))

(define-struct (nc-calc-size-params
                 (:conc-name nc-cs-params-)
                 (:constructor make-nc-cs-params
                               (rgrc pos)))
    "Contains information that an application can use while processing the WM-NC-CALC-SIZE message to calculate the size, position, and valid contents of the client area of a window."
  (rgrc (simple-array rect (3)))
  (pos (& window-pos)))

(define-struct (style-struct)
    "Contains the styles for a window."
  (old-style dword)
  (new-style dword))

(define-enum (state-system
               (:base-type dword)
               (:list t))
  (:focusable #x00100000)
  (:invisible #x00008000)
  (:offscreen #x00010000)
  (:unavailable #x00000001)
  (:pressed #x00000008))

(define-struct (title-bar-info
                 (:constructor make-title-bar-info
                               (&key title-rect
                                     title-bar
                                     minimize-button
                                     maximize-button
                                     help-button
                                     close-button)))
    "Contains title bar information."
  (cb dword :initform (sizeof 'title-bar-info))
  (title-rect rect)
  (title-bar state-system)
  (reserved dword)
  (minimize-button state-system)
  (maximize-button state-system)
  (help-button state-system)
  (close-button state-system))

(define-struct (title-bar-info*
                 (:include title-bar-info)
                 (:constructor make-title-bar-info*
                               (&key title-rect
                                     title-bar
                                     minimize-button
                                     maximize-button
                                     help-button
                                     close-button
                                     minimize-button-rect
                                     maximize-button-rect
                                     help-button-rect
                                     close-button-rect
                                &aux (cb (sizeof 'title-bar-info*)))))
    "Expands on the information described in the TITLE-BAR-INFO structure by including the coordinates of each element of the title bar."
  (reserved1 rect)
  (reserved2 rect)
  (minimize-button-rect rect)
  (maximize-button-rect rect)
  (help-button-rect rect)
  (close-button-rect rect))

(define-struct (update-layered-window-info
                 (:conc-name ulw-info-)
                 (:constructor make-ulw-info
                               (&key dest dest-pt dest-size
                                     src src-pt key blend
                                     flags dirty-rect)))
    "Used by update-layered-window-indirect to provide position, size, shape, content, and translucency information for a layered window."
  (cb dword :initform (sizeof 'update-layered-window-info))
  (dest handle)
  (dest-pt (& point :in t))
  (dest-size (& size :in t))
  (src handle)
  (src-pt (& point :in t))
  (key dword)
  (blend (& blend-function :in t))
  (flags (enum (:base-type dword)
           (:alpha 2)
           (:color-key 1)
           (:opaque 4)
           (:no-resize 8)))
  (dirty-rect (& rect :in t)))

(define-struct (window-info
                 (:conc-name winfo-)
                 (:constructor make-winfo
                               (&key window-rect
                                     client-rect
                                     style
                                     style*
                                     window-status
                                     cx-border
                                     cy-border
                                     window-type
                                     creator-version)))
    "Contains window information."
  (cb dword :initform (sizeof 'window-info))
  (window-rect rect)
  (client-rect rect)
  (style window-style)
  (style* window-style*)
  (window-status (boolean dword))
  (cx-border uint)
  (cy-border uint)
  (window-type uint16)
  (creator-version word))

(define-enum (show-command
               (:conc-name sw-)
               (:base-type dword))
  (:hide 0)
  (:maximize 3)
  (:minimize 6)
  (:restore 9)
  (:show 5)
  (:show-maximized 3)
  (:show-minimized 2)
  (:show-min-no-active 7)
  (:show-na 8)
  (:show-no-activate 4)
  (:show-normal 1))

(define-struct (window-placement
                 (:conc-name wplace-)
                 (:constructor make-wplace
                               (&key flags
                                     show
                                     min-pos
                                     max-pos
                                     norm-pos)))
    "Contains information about the placement of a window on the screen."
  (cb uint :initform (sizeof 'window-placement))
  (flags (enum (:base-type uint :list t)
           (:async-window-placement 4)
           (:restore-to-maximized 2)
           (:set-min-position 1)))
  (show show-command)
  (min-pos point)
  (max-pos point)
  (norm-pos rect))

(define-struct (bsm-info (:constructor
                           make-bsm-info (&key desk hwnd luid))
                         (:constructor bsm-info (desk hwnd luid)))
    "Contains information about a window that denied a request from broadcast-system-message*"
  (cb uint :initform (sizeof 'bsm-info))
  (desk handle)
  (hwnd handle)
  (luid doors.security:luid))

(define-struct (msg (:constructor make-msg)
                    (:constructor msg (hwnd message wparam lparam time pt)))
    "Contains message information from a thread's message queue."
  (hwnd handle)
  (message uint)
  (wparam wparam)
  (lparam lparam)
  (time dword)
  (pt point))

(define-external-function
    ("AdjustWindowRect" (:camel-case))
    (:stdcall user32)
  ((last-error bool) rv rect)
  "Calculates the required size of the window rectangle, based on the desired client-rectangle size."
  (rect (& rect :inout))
  (style window-style)
  (menu boolean :optional))

(define-external-function
    ("AdjustWindowRectEx" adjust-window-rect*)
    (:stdcall user32)
  ((last-error bool) rv rect)
  "Calculates the required size of the window rectangle, based on the desired client-rectangle size."
  (rect (& rect :inout))
  (style window-style)
  (menu boolean :optional)
  (style* window-style*))

(define-external-function
    ("AllowSetForegroundWindow" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Enables the specified process to set the foreground window using the (setf foreground-window) function."
  (process-id dword :optional current-process-id))

(define-external-function
    ("AnimateWindow" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Enables you to produce special effects when showing or hiding windows."
  (hwnd handle)
  (time dword)
  (flags (enum (:base-type dword :list t)
           (:activate #x00020000)
           (:blend #x00080000)
           (:center #x00000010)
           (:hide #x00010000)
           (:hor-positive #x00000001)
           (:hor-negative #x00000002)
           (:slide #x00040000)
           (:ver-positive #x00000004)
           (:ver-negative #x00000008))))

(define-external-function
    ("AnyPopup" (:camel-case))
    (:stdcall user32)
  (boolean)
  "Indicates whether an owned, visible, top-level pop-up, or overlapped window exists on the screen. ")

(define-external-function
    ("ArrangeIconicWindows" (:camel-case))
    (:stdcall user32)
  ((last-error uint doors::not-zero))
  "Arranges all the minimized (iconic) child windows of the specified parent window."
  (hwnd handle))

(define-external-function
    ("BeginDeferWindowPos" (:camel-case))
    (:stdcall user32)
  ((last-error handle))
  "Allocates memory for a multiple-window- position structure and returns the handle to the structure."
  (num-windows int))

(define-external-function
    ("BringWindowToTop" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Brings the specified window to the top of the Z order."
  (hwnd handle))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver winvista winserver2008)
(define-external-function
    ("CalculatePopupWindowPosition" (:camel-case))
    (:stdcall user32)
  ((last-error bool) rv pos)
  "Calculates an appropriate pop-up window position using the specified anchor point, pop-up window size, flags, and the optional exclude rectangle."
  (anchor-point (& point))
  (window-size (& size))
  (flags (enum (:base-type uint :list t)
           (:center-align #x0004)
           (:left-align #x0000)
           (:right-align #x0008)
           (:bottom-align #x0020)
           (:top-align #x0000)
           (:vcenter-align #x0010)
           (:horizontal #x0000)
           (:vertical #x0040)
           (:work-area #x10000))
         :optional)
  (exclude-rect (& rect :in t) :optional)
  (pos (& rect :out) :aux))

(define-external-function
    ("CascadeWindows" (:camel-case))
    (:stdcall user32)
  ((last-error word doors::not-zero))
  "Cascades the specified child windows of the specified parent window."
  (parent handle)
  (how (enum (:base-type uint :list t)
         (:skip-disabled 2)
         (:z-order 4))
       :key :z-order)
  (rect (& rect :in t) :key)
  (count uint :key (if (voidp kids) 0 (length kids)))
  (kids (& (~ handle) :in t)))

#-(or win2000 winxp winxp64 winhomeserver winserver2003)
(define-external-function
    ("ChangeWindowMessageFilter" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Adds or removes a message from the User Interface Privilege Isolation (UIPI) message filter."
  (message uint)
  (flag (enum (:base-type dword)
          (:add 1)
          (:remove 2))))

#-(or win2000 winxp winxp64 winhomeserver winserver2003 winvista winserver2008)
(define-external-function
    ("ChangeWindowMessageFilterEx" change-window-message-filter*)
    (:stdcall user32)
  ((last-error bool) rv change-filter-struct)
  "Modifies the User Interface Privilege Isolation (UIPI) message filter for a specified window."
  (hwnd handle)
  (message uint)
  (action (enum (:base-type dword)
            :reset
            :allow
            :disallow))  
  (change-filter-struct (& change-filter-struct :inout t) :optional))

(define-external-function
    ("ChildWindowFromPoint" (:camel-case))
    (:stdcall user32)
  (handle)
  "Determines which, if any, of the child windows belonging to a parent window contains the specified point."
  (parent handle)
  (point point))

(define-external-function
    ("ChildWindowFromPointEx" child-window-from-point*)
    (:stdcall user32)
  (handle)
  "Determines which, if any, of the child windows belonging to the specified parent window contains the specified point."
  (parent handle)
  (pt point)
  (flags (enum (:base-type uint :list t)
           (:all 0)
           (:skip-disabled 2)
           (:skip-invisible 1)
           (:skip-transparent 4))))

(define-external-function
    ("CloseWindow" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Minimizes (but does not destroy) the specified window."
  (hwnd handle))

(define-external-function
    (#+doors.unicode "CreateWindowExW"
     #-doors.unicode "CreateWindowExA"
                   create-window*)
    (:stdcall user32)
  ((last-error handle))
  "Creates an overlapped, pop-up, or child window with an extended window style; otherwise, this function is identical to the create-window function."
  (style* window-style* :key)
  (class-name (union ()
                (string (& (const tstring)))
                (uint atom))
              :key 0)
  (window-name (& tstring :in t) :key)
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
  (menu handle :key)
  (instance handle :key)
  (param pointer :key))

(define-external-function
    (#+doors.unicode "CreateWindowExW"
     #-doors.unicode "CreateWindowExA"
                   create-window)
    (:stdcall user32)
  ((last-error handle))
  "Creates an overlapped, pop-up, or child window. It specifies the window class, window title, window style, and (optionally) the initial position and size of the window."
  (style* window-style* :aux '())
  (class-name (union ()
                (string (& (const tstring)))
                (uint atom))
              :key 0)
  (window-name (& tstring :in t) :key)
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
  (menu handle :key)
  (instance handle :key)
  (param pointer :key))

(define-external-function
    ("DeferWindowPos")
    (:stdcall user32)
  ((last-error handle))
  "Updates the specified multiple-window – position structure for the specified window."
  (pos-info handle)
  (hwnd handle)
  (insert-after (union ()
                  (handle handle)
                  (enum (enum (:base-type int-ptr)
                          (:bottom 1)
                          (:no-topmost -2)
                          (:top 0)
                          (:topmost -1))))
                :optional)
  (x int)
  (y int)
  (cx int)
  (cy int)
  (flags (enum (:base-type uint :list t)
           (:draw-frame #x0020)
           (:frame-changed #x0020)
           (:hide-window #x0080)
           (:no-activate #x0010)
           (:no-copy-bits #x0100)
           (:no-move #x0002)
           (:no-owner-z-order #x0200)
           (:no-redraw #x0008)
           (:no-reposition #x0200)
           (:no-send-changing #x0400)
           (:no-size #x0001)
           (:no-z-order #x0004)
           (:show-window #x0040))))

(define-external-function
    ("DestroyWindow" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Destroys the specified window. "
  (hwnd handle))

(define-external-function
    ("EndDeferWindowPos" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Simultaneously updates the position and size of one or more windows in a single screen-refreshing cycle."
  (pos-info handle))

(define-external-function
    ("EnumChildWindows" (:camel-case))
    (:stdcall user32)
  (bool)
  "Enumerates the child windows that belong to the specified parent window by passing the handle to each child window, in turn, to an application-defined callback function."
  (parent handle)
  (enum-proc pointer)
  (param int-ptr))

(define-external-function
    ("EnumThreadWindows" (:camel-case))
    (:stdcall user32)
  (bool)
  "Enumerates all nonchild windows associated with a thread by passing the handle to each window, in turn, to an application-defined callback function."
  (thread-id dword :optional current-thread-id)
  (enum-proc pointer)
  (param int-ptr))

(define-external-function
    ("EnumWindows" (:camel-case))
    (:stdcall user32)
  (bool)
  "Enumerates all top-level windows on the screen by passing the handle to each window, in turn, to an application-defined callback function. "
  (enum-proc pointer)
  (param int-ptr))

(define-external-function
    (#+doors.unicode "FindWindowW"
     #-doors.unicode "FindWindowA"
                   find-window)
    (:stdcall user32)
  ((last-error handle))
  "Retrieves a handle to the top-level window whose class name and window name match the specified strings."
  (class-name (& tstring :in t) :key)
  (window-name (& tstring :in t) :key))

(define-external-function
    (#+doors.unicode "FindWindowExW"
     #-doors.unicode "FindWindowExA"
                   find-window*)
    (:stdcall user32)
  ((last-error handle))
  "Retrieves a handle to a window whose class name and window name match the specified strings. "
  (parent handle :key)
  (child-after handle :key)
  (class-name (& tstring :in t) :key)
  (window-name (& tstring :in t) :key))

(define-external-function
    (#+doors.unicode "GetAltTabInfoW"
     #-doors.unicode "GetAltTabInfoA"
                   alt-tab-info)
    (:stdcall user32)
  ((last-error bool) rv (values info item-text))
  "Retrieves status information for the specified window if it is the application-switching (ALT+TAB) window."
  (hwnd handle :optional)
  (item int)
  (info (& alt-tab-info :inout) :aux)
  (item-text (& tstring :out) :aux (make-string item-text-size))
  (item-text-size uint :optional 256))

(define-external-function
    ("GetAncestor" ancestor)
    (:stdcall user32)
  (handle)
  "Retrieves the handle to the ancestor of the specified window."
  (hwnd handle)
  (flags (enum (:base-type uint)
           (:parent 1)
           (:root 2)
           (:owner 3))
         :optional :parent))

(define-external-function
    ("GetClientRect" client-rect)
    (:stdcall user32)
  ((last-error bool) rv rect)
  "Retrieves the coordinates of a window's client area."
  (hwnd handle)
  (rect (& rect :out) :aux))

(define-external-function
    ("GetDesktopWindow" desktop-window)
    (:stdcall user32)
  (handle)
  "Retrieves a handle to the desktop window.")

(define-symbol-macro desktop-window (desktop-window))

(define-external-function
    ("GetForegroundWindow" foreground-window)
    (:stdcall user32)
  (handle)
  "Retrieves a handle to the foreground window (the window with which the user is currently working).")

(define-symbol-macro foreground-window (foreground-window))

(define-external-function
    ("GetGUIThreadInfo" gui-thread-info)
    (:stdcall user32)
  ((last-error bool) rv info)
  "Retrieves information about the active window or a specified GUI thread."
  (thread-id dword :optional current-thread-id)
  (info (& gui-thread-info :inout) :aux))

(define-symbol-macro gui-thread-info (gui-thread-info))

(define-external-function
    ("GetLastActivePopup" last-active-popup)
    (:stdcall user32)
  (handle)
  "Determines which pop-up window owned by the specified window was most recently active."
  (hwnd handle))

#-win2000
(define-external-function
    ("GetLayeredWindowAttributes" layered-window-attributes)
    (:stdcall user32)
  ((last-error bool) rv (values key alpha flags))
  "Retrieves the opacity and transparency color key of a layered window."
  (hwnd handle)
  (key (& dword :out) :aux)
  (alpha (& byte :out) :aux)
  (flags (& (enum (:base-type dword)
              (:alpha 2)
              (:color-key 1))
          :out)
         :aux))

(define-external-function
    ("GetWindow" next-window)
    (:stdcall user32)
  (handle)
  "Retrieves a handle to the next or previous window in the Z-Order."
  (hwnd handle)
  (cmd (enum (:base-type uint)
         (:next 2)
         (:prev 3))
       :optional :next))

(define-external-function
    ("GetParent" parent)
    (:stdcall user32)
  ((last-error handle))
  "Retrieves a handle to the specified window's parent or owner."
  (hwnd handle))

(define-external-function
    ("GetProcessDefaultLayout" process-default-layout)
    (:stdcall user32)
  ((last-error bool) rv layout)
  "Retrieves the default layout that is used when windows are created with no parent or owner."
  (layout (& (boolean dword) :out) :aux))

(define-symbol-macro process-default-layout (process-default-layout))

(define-external-function
    ("GetShellWindow" shell-window)
    (:stdcall user32)
  (handle)
  "Retrieves a handle to the Shell's desktop window.")

(define-symbol-macro shell-window (shell-window))

(define-external-function
    ("GetSysColor" system-color)
    (:stdcall user32)
  (dword)
  "Retrieves the current color of the specified display element."
  (index system-color))

(define-external-function
    ("GetTitleBarInfo" title-bar-info)
    (:stdcall user32)
  ((last-error bool) rv info)
  "Retrieves information about the specified title bar."
  (hwnd handle)
  (info (& title-bar-info :inout) :aux))

(define-external-function
    ("GetTopWindow" top-window)
    (:stdcall user32)
  ((last-error handle))
  "Examines the Z order of the child windows associated with the specified parent window and retrieves a handle to the child window at the top of the Z order."
  (hwnd handle :optional))

(define-symbol-macro top-window (top-window))

(define-external-function
    ("GetWindow" get-window)
    (:stdcall user32)
  ((last-error handle))
  "Retrieves a handle to a window that has the specified relationship (Z-Order or owner) to the specified window."
  (hwnd handle)
  (cmd (enum (:base-type uint)
         (:child 5)
         (:enabled-popup 6)
         (:first 0)
         (:last 1)
         (:next 2)
         (:prev 3)
         (:owner 4))))

#-(or win2000 winxp winxp64 winhomeserver winserver2003 winvista winserver2008)
(define-external-function
    ("GetWindowDisplayAffinity" window-display-affinity)
    (:stdcall user32)
  ((last-error bool) rv affinity)
  "Retrieves the current display affinity setting, from any process, for a given window."
  (hwnd handle)
  (affinity (& (boolean dword) :out) :aux))

(define-external-function
    ("GetWindowInfo" window-info)
    (:stdcall user32)
  ((last-error bool) rv info)
  "Retrieves information about the specified window."
  (hwnd handle)
  (info (& window-info :out) :aux))

(define-external-function
    (#+doors.unicode "GetWindowModuleFileNameW"
     #-doors.unicode "GetWindowModuleFileNameA"
                   window-module-file-name)
    (:stdcall user32)
  ((last-error uint doors::not-zero) rv
   (subseq buffer 0 rv))
  "Retrieves the full path and file name of the module associated with the specified window handle."
  (hwnd handle)
  (buffer (& tstring :out) :aux (make-string size))
  (size uint :optional 256))

(define-external-function
    ("GetWindowPlacement" window-placement)
    (:stdcall user32)
  ((last-error bool) rv info)
  "Retrieves the show state and the restored, minimized, and maximized positions of the specified window."
  (hwnd handle)
  (info (& window-placement :inout) :aux))

(define-external-function
    ("GetWindowRect" window-rect)
    (:stdcall user32)
  ((last-error bool) rv rect)
  "Retrieves the dimensions of the bounding rectangle of the specified window."
  (hwnd handle)
  (rect (& rect :out) :aux))

(define-external-function
    (#+doors.unicode "GetWindowTextLengthW"
     #-doors.unicode "GetWindowTextLengthA"
                   window-text-length)
    (:stdcall user32)
  (int)
  "Retrieves the length, in characters, of the specified window's title bar text (if the window has a title bar)." 
  (hwnd handle))

(define-external-function
    (#+doors.unicode "GetWindowTextW"
     #-doors.unicode "GetWindowTextA"
                   window-text)
    (:stdcall user32)
  ((last-error int doors::not-zero) rv
   (if (= rv (1- count))
     buffer
     (subseq buffer 0 rv)))
  "Copies the text of the specified window's title bar (if it has one) into a buffer."
  (hwnd handle)
  (buffer (& tstring :out) :aux (make-string (1- count)))
  (count int :optional (1+ (window-text-length hwnd))))

(define-external-function
    ("GetWindowThreadProcessId" window-thread-process-id)
    (:stdcall user32)
  ((last-error dword doors::not-zero) rv id)
  "Retrieves the identifier of the thread that created the specified window and, optionally, the identifier of the process that created the window."
  (hwnd handle)
  (id (& dword :out) :aux))

(define-external-function
    ("IsChild" (:camel-case))
    (:stdcall user32)
  (bool)
  "Determines whether a window is a child window or descendant window of a specified parent window. "
  (parent handle)
  (hwnd handle))

(define-external-function
    ("IsGUIThread" is-gui-thread)
    (:stdcall user32)
  (bool)
  "Determines whether the calling thread is already a GUI thread. It can also optionally convert the thread to a GUI thread."
  (convert bool :optional))

(define-external-function
    ("IsIconic" (:camel-case))
    (:stdcall user32)
  (bool)
  "Determines whether the specified window is minimized (iconic)."
  (hwnd handle))

#-(or win2000 winxp winxp64 winhomeserver winserver2003)
(define-external-function
    ("IsProcessDPIAware" is-process-dpi-aware)
    (:stdcall user32)
  (bool)
  "Determines whether the current process is dots per inch (dpi) aware such that it adjusts the sizes of UI elements to compensate for the dpi setting.")

(define-external-function
    ("IsWindow" (:camel-case))
    (:stdcall user32)
  (bool)
  "Determines whether the specified window handle identifies an existing window."
  (handle handle))

(define-external-function
    ("IsWindowUnicode" (:camel-case))
    (:stdcall user32)
  (bool)
  "Determines whether the specified window is a native Unicode window."
  (hwnd handle))

(define-external-function
    ("IsWindowVisible" (:camel-case))
    (:stdcall user32)
  (bool)
  "Determines the visibility state of the specified window."
  (hwnd handle))

(define-external-function
    ("IsZoomed" (:camel-case))
    (:stdcall user32)
  (bool)
  "Determines whether a window is maximized."
  (hwnd handle))

(define-external-function
    ("LockSetForegroundWindow" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "The foreground process can call the lock-set-foreground-window function to disable calls to the (setf foreground-window) function."
  (lock-code (enum (:base-type uint)
               (:lock 1)
               (:unlock 2))
             :optional :lock))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("LogicalToPhysicalPoint" (:camel-case))
    (:stdcall user32)
  ((last-error bool) rv point)
  "Converts the logical coordinates of a point in a window to physical coordinates."
  (hwnd handle)
  (point (& point :inout)))

(define-external-function
    ("MoveWindow" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Changes the position and dimensions of the specified window."
  (hwnd handle)
  (x int)
  (y int)
  (width int)
  (height int)
  (repaint bool :optional t))

(define-external-function
    ("OpenIcon" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Restores a minimized (iconic) window to its previous size and position; it then activates the window."
  (hwnd handle))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("PhysicalToLogicalPoint" (:camel-case))
    (:stdcall user32)
  ((last-error bool) rv point)
  "Converts the physical coordinates of a point in a window to logical coordinates."
  (hwnd handle)
  (point (& point :inout)))

(define-external-function
    ("RealChildWindowFromPoint" (:camel-case))
    (:stdcall user32)
  (handle)
  "Retrieves a handle to the child window at the specified point."
  (parent handle)
  (client-coords point))

(define-external-function
    (#+doors.unicode "RealGetWindowClassW"
     #-doors.unicode "RealGetWindowClassA"
                   real-window-class)
    (:stdcall user32)
  ((last-error uint doors::not-zero) rv
   (subseq buffer 0 rv))
  "Retrieves a string that specifies the window type."
  (hwnd handle)
  (buffer (& tstring :out) :aux (make-string buffer-length))
  (buffer-length uint :optional 256))

(define-external-function
    ("SetForegroundWindow" (setf foreground-window))
    (:stdcall user32)
  (bool)
  "Brings the thread that created the specified window into the foreground and activates the window."
  (hwnd handle))

(define-external-function
    ("SetLayeredWindowAttributes" (setf layered-window-attributes))
    (:stdcall user32)
  ((last-error bool) rv)
  "Sets the opacity and transparency color key of a layered window."
  (hwnd handle)
  (color-key dword :key)
  (alpha byte :key 255)
  (flags (enum (:base-type dword)
           (:alpha 2)
           (:color-key 1))))

(define-external-function
    ("SetParent" (setf parent))
    (:stdcall user32)
  ((last-error handle))
  "Changes the parent window of the specified child window."
  (child handle :optional)
  (new-parent (union ()
                 (handle handle)
                 (uint (enum (:base-type int-ptr)
                         (:message-window -3))))))

(define-external-function
    ("SetProcessDefaultLayout" (setf process-default-layout))
    (:stdcall user32)
  ((last-error bool))
  "Changes the default layout when windows are created with no parent or owner only for the currently running process."
  (default-layout (boolean dword)))

(define-external-function
    ("SetProcessDPIAware" set-process-dpi-aware)
    (:stdcall user32)
  ((last-error bool))
  "Sets the current process as dots per inch (dpi) aware.")

(define-external-function
    ("SetSysColors" set-system-colors)
    (:stdcall user32)
  ((last-error bool))
  "Sets the colors for the specified display elements."
  (count int :optional (min (array-total-size elements)
                            (array-total-size rgb-values)))
  (elements (& (array system-color)))
  (rgb-values (& (array dword))))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver winvista winserver2008)
(define-external-function
    ("SetWindowDisplayAffinity" (setf window-display-affinity))
    (:stdcall user32)
  ((last-error bool) rv affinity)
  "Stores the display affinity setting in kernel mode on the hWnd associated with the window."
  (hwnd handle :optional)
  (affinity (boolean dword)))

(define-external-function
    ("SetWindowPlacement" (setf window-placement))
    (:stdcall user32)
  ((last-error bool) rv placement)
  "Sets the show state and the restored, minimized, and maximized positions of the specified window."
  (hwnd handle :optional)
  (placement (& window-placement)))

(define-external-function
    ("SetWindowPos" set-window-pos)
    (:stdcall user32)
  ((last-error bool))
  "Changes the size, position, and Z order of a child, pop-up, or top-level window."
  (hwnd handle)
  (insert-after (union ()
                  (handle handle)
                  (enum (enum (:base-type int-ptr)
                          (:bottom 1)
                          (:no-topmost -2)
                          (:top 0)
                          (:topmost -1))))
                :optional)
  (x int)
  (y int)
  (cx int)
  (cy int)
  (flags (enum (:base-type uint :list t)
           (:async-window-pos #x4000)
           (:defer-erase #x2000)
           (:draw-frame #x0020)
           (:frame-changed #x0020)
           (:hide-window #x0080)
           (:no-activate #x0010)
           (:no-copy-bits #x0100)
           (:no-move #x0002)
           (:no-owner-z-order #x0200)
           (:no-redraw #x0008)
           (:no-reposition #x0200)
           (:no-send-changing #x0400)
           (:no-size #x0001)
           (:no-z-order #x0004)
           (:show-window #x0040))))

(define-external-function
    (#+doors.unicode "SetWindowTextW"
     #-doors.unicode "SetWindowTextA"
                   (setf window-text))
    (:stdcall user32)
  ((last-error bool) rv string)
  "Changes the text of the specified window's title bar (if it has one)."
  (hwnd handle :optional)
  (string (& tstring)))

(define-external-function
    ("ShowOwnedPopups" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Shows or hides all pop-up windows owned by the specified window."
  (hwnd handle)
  (show bool :optional t))

(define-external-function
    ("ShowWindow" (:camel-case))
    (:stdcall user32)
  (bool)
  "Sets the specified window's show state."
  (hwnd handle)
  (show-command show-command :optional :show-normal))

(define-external-function
    ("ShowWindowAsync" (:camel-case))
    (:stdcall user32)
  (bool)
  "Sets the show state of a window created by a different thread."
  (hwnd handle)
  (show-command show-command :optional :show-normal))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("SoundSentry" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Triggers a visual signal to indicate that a sound is playing.")

(define-external-function
    ("TileWindows" (:camel-case))
    (:stdcall user32)
  ((last-error word doors::not-zero))
  "Tiles the specified child windows of the specified parent window."
  (parent handle)
  (how (enum (:base-type uint)
         (:horizontal 1)
         (:vertical 0))
       :key)
  (rect (& rect :in t) :key)
  (count uint :key (if (voidp kids) 0 (length kids)))
  (kids (& (~ handle) :in t)))

(define-external-function
    ("UpdateLayeredWindow" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Updates the position, size, shape, content, and translucency of a layered window."
  (hwnd handle)
  (dest handle :key)
  (dest-pt (& point :in t) :key)
  (size (& size :in t) :key)
  (src handle :key)
  (src-pt (& point :in t) :key)
  (key dword :key)
  (blend (& blend-function :in t) :key)
  (flags (enum (:base-type dword)
           (:color-key 1)
           (:alpha 2)
           (:opaque 4))))

#-(or win2000 winxp winxp64 winhomeserver winserver2003)
(define-external-function
    ("UpdateLayeredWindowIndirect" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Updates the position, size, shape, content, and translucency of a layered window."
  (hwnd handle)
  (ulw-info (& update-layered-window-info)))

#-(or win2000 winxp winxp64 winhomeserver winserver2003)
(define-external-function
    ("WindowFromPhysicalPoint" (:camel-case))
    (:stdcall user32)
  (handle)
  "Retrieves a handle to the window that contains the specified physical point."
  (point point))

(define-external-function
    ("WindowFromPoint" (:camel-case))
    (:stdcall user32)
  (handle)
  "Retrieves a handle to the window that contains the specified point."
  (point point))

(define-external-function
    (#+doors.unicode "CallWindowProcW"
     #-doors.unicode "CallWindowProcA"
                   call-window-proc)
    (:stdcall user32)
  (lresult)
  "Passes message information to the specified window procedure."
  (prev-wnd-func pointer)
  (hwnd handle)
  (msg uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    (#+doors.unicode "DefWindowProcW"
     #-doors.unicode "DefWindowProcA"
                   def-window-proc)
    (:stdcall user32)
  (lresult)
  "Calls the default window procedure to provide default processing for any window messages that an application does not process."
  (hwnd handle)
  (msg uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    ("BroadcastSystemMessageW" broadcast-system-message)
    (:stdcall user32)
  ((last-error long doors::not-zero) rv (values (/= -1 rv) recipients))
  "Sends a message to the specified recipients."
  (flags (enum (:base-type dword :list t)
           (:allow-sfw #x00000080)
           (:flush-disk #x00000004)
           (:force-if-hung #x00000020)
           (:ignore-current-task #x00000002)
           (:no-hang #x00000008)
           (:no-timeout-if-not-hung #x00000040)
           (:post-message #x00000010)
           (:query #x00000001)
           (:send-notify-message #x00000100)))
  (recipients (& (enum (:base-type dword :list t)
                   (:all-components #x00000000)
                   (:all-desktops #x00000010)
                   (:applications #x00000008))
               :inout)
              :optional)
  (message uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    (#+doors.unicode "BroadcastSystemMessageExW"
     #-doors.unicode "BroadcastSystemMessageExA"
                   broadcast-system-message*)
    (:stdcall user32)
  ((last-error long doors::not-zero) rv (values (/= -1 rv) recipients info))
  "Sends a message to the specified recipients."
  (flags (enum (:base-type dword :list t)
           (:allow-sfw #x00000080)
           (:flush-disk #x00000004)
           (:force-if-hung #x00000020)
           (:ignore-current-task #x00000002)
           (:no-hang #x00000008)
           (:no-timeout-if-not-hung #x00000040)
           (:post-message #x00000010)
           (:query #x00000001)
           (:send-notify-message #x00000100)))
  (recipients (& (enum (:base-type dword :list t)
                   (:all-components #x00000000)
                   (:all-desktops #x00000010)
                   (:applications #x00000008))
               :inout)
              :optional)
  (message uint)
  (wparam wparam)
  (lparam lparam)
  (info (& bsm-info :out) :aux))

(define-external-function
    (#+doors.unicode "DispatchMessageW"
     #-doors.unicode "DispatchMessageA"
                   dispatch-message)
    (:stdcall user32)
  (lresult)
  "Dispatches a message to a window procedure."
  (msg (& msg)))

(define-external-function
    ("GetInputState" input-state)
    (:stdcall user32)
  (bool)
  "Determines whether there are mouse-button or keyboard messages in the calling thread's message queue.")

(define-symbol-macro input-state (input-state))

(define-external-function
    (#+doors.unicode "GetMessageW"
     #-doors.unicode "GetMessageA"
                   get-message)
    (:stdcall user32)
  (int rv (cond
            ((> rv 0) msg)
            ((= rv 0) nil)
            (T (invoke-last-error))))
  "Retrieves a message from the calling thread's message queue."
  (msg (& msg :out) :optional)
  (hwnd handle :optional)
  (filter-min uint :optional 0)
  (filter-max uint :optional 0))

(define-external-function
    ("GetMessageExtraInfo" message-extra-info)
    (:stdcall user32)
  (lparam)
  "Retrieves the extra message information for the current thread.")

(define-symbol-macro message-extra-info (message-extra-info))

(define-external-function
    ("GetMessagePos" message-pos)
    (:stdcall user32)
  (point)
  "Retrieves the cursor position for the last message retrieved by the get-message function.")

(define-symbol-macro message-pos (message-pos))

(define-external-function
    ("GetMessageTime" message-time)
    (:stdcall user32)
  (long)
  "Retrieves the message time for the last message retrieved by the get-message function.")

(define-symbol-macro message-time (message-time))

(define-enum (queue-status
               (:base-type uint)
               (:conc-name qs-))
  (:all-post-message #x0100)
  (:hotkey #x0080)
  (:key #x0001)
  (:mouse-button #x0004)
  (:mouse-move #x0002)
  (:paint #x0020)
  (:post-message #x0008)
  (:raw-input #x0400)
  (:send-message #x0040)
  (:timer #x0010)
  (:mouse #x0006)
  (:input #x0407)
  (:all-input #x04FF)
  (:all-events #x04BF))

(define-external-function
    ("GetQueueStatus" queue-status)
    (:stdcall user32)
  (dword rv (values (translate (low-word rv) 'queue-status)
                    (translate (high-word rv) 'queue-status)))
  "Retrieves the type of messages found in the calling thread's message queue."
  (flags queue-status))

(define-external-function
    ("InSendMessage" in-send-message)
    (:stdcall user32)
  (bool)
  "Determines whether the current window procedure is processing a message that was sent from another thread (in the same process or a different process) by a call to the send-message function.")

(define-symbol-macro in-send-message (in-send-message))

(define-external-function
    ("InSendMessageEx" in-send-message*)
    (:stdcall user32)
  (dword rv (if (zerop rv)
              nil
              (translate rv '(enum (:base-type dword :list t)
                              (:callback #x00000004)                              
                              (:notify #x00000002)
                              (:replied #x00000008)
                              (:send #x00000001)))))
  "Determines whether the current window procedure is processing a message that was sent from another thread (in the same process or a different process)."
  (reserved pointer :aux &0))

(define-symbol-macro in-send-message* (in-send-message*))

(define-external-function
    (#+doors.unicode "PeekMessageW"
     #-doors.unicode "PeekMessageA"
                   peek-message)
    (:stdcall user32)
  (bool rv (if rv
             msg
             nil))
  "Dispatches incoming sent messages, checks the thread message queue for a posted message, and retrieves the message (if any exist)."
  (msg (& msg :out) :optional)
  (hwnd handle :optional)
  (filter-min uint :optional 0)
  (filter-max uint :optional 0)
  (remove-msg (enum (:base-type uint)
                (:no-remove 0)
                (:remove 1)
                (:no-yield 2))
              :optional 1))

(define-external-function
    (#+doors.unicode "PostMessageW"
     #-doors.unicode "PostMessageA"
                   post-message)
    (:stdcall user32)
  ((last-error bool))
  "Places (posts) a message in the message queue associated with the thread that created the specified window and returns without waiting for the thread to process the message."
  (hwnd (union ()
          (handle handle)
          (enum (enum (:base-type uint-ptr)
                  (:broadcast #xFFFF))))
        :optional &0)
  (msg uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    ("PostQuitMessage" post-quit-message)
    (:stdcall user32)
  (void)
  "Indicates to the system that a thread has made a request to terminate (quit). It is typically used in response to a WM-DESTROY message."
  (exit-code int :optional 0))

(define-external-function
    (#+doors.unicode "PostThreadMessageW"
     #-doors.unicode "PostThreadMessageA"
                   post-thread-message)
    (:stdcall user32)
  ((last-error bool))
  "Posts a message to the message queue of the specified thread. It returns without waiting for the thread to process the message."
  (thread-id dword :optional current-thread-id)
  (msg uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    (#+doors.unicode "RegisterWindowMessageW"
     #-doors.unicode "RegisterWindowMessageA"
                   register-window-message)
    (:stdcall user32)
  (uint)
  "Defines a new window message that is guaranteed to be unique throughout the system. The message value can be used when sending or posting messages."
  (string (& tstring)))

(define-external-function
    ("ReplyMessage" (:camel-case))
    (:stdcall user32)
  (bool)
  "Replies to a message sent through the send-message function without returning control to the function that called send-message."
  (lresult lresult))

(define-external-function
    (#+doors.unicode "SendMessageW"
     #-doors.unicode "SendMessageA"
                   send-message)
    (:stdcall user32)
  (lresult)
  "Sends the specified message to a window or windows."
  (hwnd (union ()
          (handle handle)
          (enum (enum (:base-type uint-ptr)
                  (:broadcast #xFFFF)))))
  (msg uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    (#+doors.unicode "SendMessageCallbackW"
     #-doors.unicode "SendMessageCallbackA"
                   send-message-callback)
    (:stdcall user32)
  ((last-error bool))
  "Sends the specified message to a window or windows."
  (hwnd (union ()
          (handle handle)
          (enum (enum (:base-type uint-ptr)
                  (:broadcast #xFFFF)))))
  (msg uint)
  (wparam wparam)
  (lparam lparam)
  (callback pointer)
  (data ulong-ptr :optional))

(define-external-function
    (#+doors.unicode "SendMessageTimeoutW"
     #-doors.unicode "SendMessageTimeoutA"
                   send-message-timeout)
    (:stdcall user32)
  ((last-error (boolean lresult)) rv result)
  "Sends the specified message to one or more windows."
  (hwnd (union ()
          (handle handle)
          (enum (enum (:base-type uint-ptr)
                  (:broadcast #xFFFF)))))
  (msg uint)
  (wparam wparam)
  (lparam lparam)
  (flags (enum (:base-type uint)
           (:abort-if-hung #x0002)
           (:block #x0001)
           (:normal #x0000)
           (:no-timeout-if-not-hung #x0008)
           (:error-on-exit #x0020))
         :optional 0)
  (timeout uint)
  (result (& uint-ptr :out) :aux))

(define-external-function
    (#+doors.unicode "SendNotifyMessageW"
     #-doors.unicode "SendNotifyMessageA"
                   send-notify-message)
    (:stdcall user32)
  ((last-error bool))
  "Sends the specified message to a window or windows."
  (hwnd (union ()
          (handle handle)
          (enum (enum (:base-type uint-ptr)
                  (:broadcast #xFFFF)))))
  (msg uint)
  (wparam wparam)
  (lparam lparam))

(define-external-function
    ("SetMessageExtraInfo" (setf message-extra-info))
    (:stdcall user32)
  (lparam)
  "Sets the extra message information for the current thread."
  (lparam lparam))

(define-external-function
    ("TranslateMessage" (:camel-case))
    (:stdcall user32)
  (bool)
  "Translates virtual-key messages into character messages."
  (msg (& msg)))


(define-external-function
    ("WaitMessage" (:camel-case))
    (:stdcall user32)
  ((last-error bool))
  "Yields control to other threads when a thread has no other messages in its message queue.")

