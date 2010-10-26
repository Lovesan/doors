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

(define-enum (console-event
               (:conc-name event-console-))
  (:caret #x4001)
  (:end-application #x4007)
  (:layout #x4005)
  (:start-application #x4006)
  (:update-region #x4002)
  (:update-scroll #x4004)
  (:update-simple #x4003))

(define-enum (char-attributes
               (:conc-name char-))
  (:foreground-blue #x0001)
  (:foreground-green #x0002)
  (:foreground-red #x0004)
  (:foreground-intensity #x0008)
  (:background-blue #x0010)
  (:background-green #x0020)
  (:background-red   #x0040)
  (:background-intensity #x0080)
  (:common-lvb-leading-byte #x0100)
  (:common-lvb-trailing-byte #x0200)
  (:common-lvb-grid-horizontal #x0400)
  (:common-lvb-grid-lvertical #x0800)
  (:common-lvb-grid-rvertical #x1000)
  (:common-lvb-reverse-video #x4000)
  (:common-lvb-underscore #x8000))

(define-struct (char-info
                 (:constructor char-info
                               (char attributes)))
    "Specifies a Unicode or ANSI character and its attributes."
  (char wchar)
  (attributes char-attributes))

(defmethod print-object ((object char-info) stream)
  (if *print-readably*
    (call-next-method)
    (print-unreadable-object (object stream)
      (format stream "~a ~{~[~:;~:*~s~^ ~]~}"
              (char-info-char object)
              (let ((attrs (convert (char-info-attributes object)
                                    'char-attributes)))
                (loop :for i :below 16
                  :collect (translate (logand attrs (ash 1 i))
                                      'char-attributes))))
      object)))

(define-struct (console-cursor-info                 
                 (:conc-name console-cursor-))
    "Contains information about the console cursor."
  (size dword)
  (visible bool))

(declaim (inline coord coord-x coord-y (setf coord-x) (setf coord-y)))
(define-struct (coord
                 (:constructor coord (x y)))
    "Defines the coordinates of a character cell in a console screen buffer. "
  (x short)
  (y short))

(declaim (inline coord-to-dword))
(defun coord-to-dword (coord)
  (declare (type coord coord))
  (logior (logand #xFFFF (coord-x coord))
          (ash (logand #xFFFF (coord-y coord)) 16)))

(declaim (inline coord-from-dword))
(defun coord-from-dword (dword)
  (declare (type dword dword))
  (coord (let ((x (ldb (byte 16 0) dword)))
           (if (> x 32767)             
             (lognot (logand #xFFFF (lognot x)))
             x))
         (let ((x (ldb (byte 16 16) dword)))
           (if (> x 32767)             
             (lognot (logand #xFFFF (lognot x)))
             x))))

(define-struct (console-font-info                 
                 (:conc-name console-))
    "Contains information for a console font."
  (font dword)
  (font-size coord))


(define-struct (console-font-info-ex
                 (:conc-name console-ex-))
    "Contains extended information for a console font."
  (font-info-struct-size ulong :initform (sizeof 'console-font-info-ex))
  (font dword)
  (font-size coord)
  (font-family uint)
  (font-weight uint)
  (face-name (wstring 32)))

(define-struct (console-history-info
                 (:conc-name console-history-))
    "Contains information about the console history."
  (size uint :initform (sizeof 'console-history-info))
  (buffer-size uint)
  (number-of-history-buffers uint)
  (flags (enum (:base-type dword)
           (:no-dup #x1))))

(define-enum (control-key-state
               (:conc-name control-key-))
  (:capslock-on #x0080)
  (:enhanced-key #x0100)
  (:left-alt-pressed #x0002)
  (:left-ctrl-pressed #x0008)
  (:numlock-on #x0020)
  (:right-alt-pressed #x0001)
  (:right-ctrl-pressed #x0004)
  (:scrolllock-on #x0040)
  (:shift-pressed #x0010))

(define-struct (console-read-control
                 (:constructor make-console-read-control
                               (&key initial-chars
                                     wakeup-mask
                                     control-key-state))
                 (:conc-name console-rc-))
    "Contains information for a console read operation."
  (length ulong :initform (sizeof 'console-read-control))
  (initial-chars ulong)
  (wakeup-mask ulong)
  (control-key-state control-key-state))

(define-struct (small-rect
                 (:constructor small-rect (left top right bottom)))
    "Defines the coordinates of the upper left and lower right corners of a rectangle."
  (left short)
  (top short)
  (right short)
  (bottom short))

(define-struct (console-screen-buffer-info
                 (:conc-name console-sb-))
    "Contains information about a console screen buffer."
  (size coord)
  (cursor-position coord)
  (attributes char-attributes)
  (window small-rect)
  (maximum-window-size coord))

(define-struct (console-screen-buffer-info-ex
                 (:constructor make-console-screen-buffer-info-ex
                               (&key size cursor-position attributes
                                window maximum-window-size popup-attributes
                                fill-screen-supported color-table))
                 (:conc-name console-sb-ex-))
    "Contains extended information about a console screen buffer."
  (struct-size ulong
               :initform (sizeof 'console-screen-buffer-info-ex))
  (size coord)
  (cursor-position coord)
  (attributes char-attributes)
  (window small-rect)
  (maximum-window-size coord)
  (popup-attributes char-attributes)
  (fill-screen-supported bool)
  (color-table (simple-array dword (16))))

(define-enum (console-selection-flags
               (:base-type dword)
               (:conc-name console-))
  (:mouse-down #x8)  
  (:mouse-selection #x4)
  (:no-selection #x0)
  (:selection-in-progress #x1)
  (:selection-not-empty #x2))

(define-struct (console-selection-info
                 (:conc-name console-))
    "Contains information for a console selection."
  (selection-flags console-selection-flags)
  (selection-anchor coord)
  (selection small-rect))

(define-struct (focus-event-record
                 (:conc-name focus-event-))
    "Describes a focus event in a console."
    (set-focus bool))

(define-struct (key-event-record
                 (:conc-name key-event-))
    "Describes a keyboard input event in a console."
  (key-down bool)
  (repeat-count word)
  (virtual-key-code word)
  (char wchar)
  (control-key-state control-key-state))

(define-struct (menu-event-record
                 (:conc-name menu-event-)
                 (:constructor make-menu-event-record ()))
    "Describes a menu event in a console. These events are used internally and should be ignored."
  (command-id uint))

(define-struct (mouse-event-record
                 (:conc-name mouse-event-))
    "Describes a mouse input event in a console."
  (mouse-position coord)
  (button-state (enum (:base-type dword)
                      (:from-left-1st-button-pressed #x1)
                      (:from-left-2nd-button-pressed #x4)
                      (:from-left-3rd-button-pressed #x8)
                      (:from-left-4th-button-pressed #x10)
                      (:rightmost-button-pressed #x2)))
  (control-key-state control-key-state)
  (flags (enum (:base-type dword)
               (:double-click 2)
               (:hwheeled 8)
               (:moved 1)
               (:vwheeled 4))))

(define-struct (window-buffer-size-record
                 (:conc-name window-buffer-event-))
    "Describes a change in the size of the console screen buffer."
    (size coord))

(define-struct (input-record
                 (:reader %input-record-reader))
    "Describes an input event in the console input buffer. "
  (event-type (enum (:base-type word)
                    (:focus-event #x10)
                    (:key-event #x1)
                    (:menu-event #x8)
                    (:mouse-event #x2)
                    (:window-buffer-size-event #x4)))
  (event (union ()
                (key-event key-event-record)
                (mouse-event mouse-event-record)
                (window-buffer-size-event window-buffer-size-record)
                (menu-event menu-event-record)
                (focus-event focus-event-record))))

(defun %input-record-reader (p out)
  (declare (type pointer p))
  (let* ((event-type (deref p '(enum (:base-type word)
                                (:focus-event #x10)
                                (:key-event #x1)
                                (:menu-event #x8)
                                (:mouse-event #x2)
                                (:window-buffer-size-event #x4))))
         (out (or out (make-input-record))))
    (declare (type input-record out))
    (setf (input-record-event-type out) event-type
          (input-record-event out)
          (case event-type
            (:key-event (deref p 'key-event-record
                               (offsetof 'input-record 'event)))
            (:focus-event (deref p 'focus-event-record
                                 (offsetof 'input-record 'event)))
            (:mouse-event (deref p 'mouse-event-record
                                 (offsetof 'input-record 'event)))
            (:window-buffer-size-event
              (deref p 'window-buffer-size-record
                     (offsetof 'input-record 'event)))
            (T (deref p 'menu-event-record
                      (offsetof 'input-record 'event)))))
    out))

(define-enum (std-handle
               (:base-type dword)
               (:conc-name std-))
  (:input-handle (logand #xFFFFFFFF -10))
  (:output-handle (logand #xFFFFFFFF -11))
  (:error-handle (logand #xFFFFFFFF -12)))

(define-external-function
    ("GetStdHandle" std-handle)
    (:stdcall kernel32)
  ((last-error handle valid-handle-p))
  "Retrieves a handle to the specified standard device (standard input, standard output, or standard error)."
  (std-handle std-handle))

(define-external-function
    (#+doors.unicode "AddConsoleAliasA"
     #-doors.unicode "AddConsoleAliasW"
                   add-console-alias)
    (:stdcall kernel32)
  ((last-error boolean))
  "Defines a console alias for the specified executable."
  (source (& tstring))
  (target (& tstring :in t))
  (exe-name (& tstring)))

(define-external-function
    ("AllocConsole" (:camel-case))
    (:stdcall kernel32)  
  ((last-error boolean))
  "Allocates a new console for the calling process.")

#-win2000
(define-external-function
    ("AttachConsole" (:camel-case))
    (:stdcall kernel32)
  ((last-error boolean))
  "Attaches the calling process to the console of the specified process."
  (process-id (enum (:base-type dword)
                (:attach-parent-process #xFFFFFFFF))))

(define-external-function
    ("CreateConsoleScreenBuffer" (:camel-case))
    (:stdcall kernel32)
  ((last-error handle valid-handle-p))
  "Creates a console screen buffer."
  (desired-access (enum (:base-type dword)
                        (:generic-read  #x80000000)
                        (:generic-write #x40000000))
                  :key #xC0000000)
  (share-mode (enum (:base-type dword)
                    (:file-share-read 1)
                    (:file-share-write 2))
              :key 0)
  (security-attributes (& security-attributes :in t) :key void)
  (flags dword :aux 1)
  (screen-buffer-data pointer :aux &0))

(define-external-function
    ("FillConsoleOutputAttribute" (:camel-case))
    (:stdcall kernel32)
  ((last-error boolean) rv number-of-attrs-written)
  "Sets the character attributes for a specified number of character cells, beginning at the specified coordinates in a screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (attribute char-attributes)
  (length dword)
  (write-coord dword)
  (number-of-attrs-written (& dword :out) :aux))

(define-external-function
    (#+doors.unicode "FillConsoleOutputCharacterW"
     #-doors.unicode "FillConsoleOutputCharacterA"
                   fill-console-output-character)
    (:stdcall kernel32)
  ((last-error bool) rv chars-written)
  "Writes a character to the console screen buffer a specified number of times, beginning at the specified coordinates."
  (console-output handle :optional (std-handle :output-handle))
  (character tchar)
  (length dword)
  (write-coord dword)
  (chars-written (& dword :out) :aux))

(define-external-function
    ("FlushConsoleInputBuffer" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool))
  "Flushes the console input buffer. All input records currently in the input buffer are discarded."
  (console-input handle :optional (std-handle :input-handle)))

(define-external-function
    ("FreeConsole" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool))
  "Detaches the calling process from its console.")

(define-external-function
    ("GenerateConsoleCtrlEvent" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool))
  "Sends a specified signal to a console process group that shares the console associated with the calling process."
  (ctrl-event (enum (:base-type dword)
                    :ctrl-c-event
                    :ctrl-break-event))
  (process-group-id dword :optional 0))

(define-external-function
    (#+doors.unicode "GetConsoleAliasW"
     #-doors.unicode "GetConsoleAliasA"
                   get-console-alias)
    (:stdcall kernel32)
  ((last-error dword not-zero) rv  target-buffer)
  "Retrieves the text for the specified console alias and executable."
  (source (& tstring))
  (target-buffer (& tstring :out)
                 :aux (make-string buffer-length))
  (buffer-length dword)
  (exe-name (& tstring)))

(define-external-function
    (#+doors.unicode "GetConsoleAliasesLengthW"
     #-doors.unicode "GetConsoleAliasesLengthA"
                   get-console-aliases-length)
    (:stdcall kernel32)
  (dword)
  "Retrieves the required size for the buffer used by the get-console-aliases function."
  (exe-name (& tstring)))

(define-external-function
    (#+doors.unicode "GetConsoleAliasesW"
     #-doors.unicode "GetConsoleAliasesA"
                   get-console-aliases)
    (:stdcall kernel32)
  ((last-error dword not-zero) rv alias-buffer)
  "Retrieves all defined console aliases for the specified executable."
  (alias-buffer (& tstring :out)
                :aux (make-string buffer-length))
  (buffer-length dword :optional (get-console-aliases-length exe-name))
  (exe-name (& tstring)))

(define-external-function
    (#+doors.unicode "GetConsoleAliasExesLengthW"
     #-doors.unicode "GetConsoleAliasExesLengthA"
                   get-console-alias-exes-length)
    (:stdcall kernel32)
  (dword)
  "Retrieves the required size for the buffer used by the get-console-alias-exes function.")

(define-external-function
    (#+doors.unicode "GetConsoleAliasExesW"
     #-doors.unicode "GetConsoleAliasExesA"
                   get-console-alias-exes)
    (:stdcall kernel32)
  ((last-error dword not-zero) rv exe-name-buffer)
  "Retrieves the names of all executable files with console aliases defined."
  (exe-name-buffer (& tstring :out)
                   :aux (make-string buffer-length))
  (buffer-length dword :optional (get-console-alias-exes-length)))

(define-external-function
    ("GetConsoleCP" console-input-code-page)
    (:stdcall kernel32)
  (uint)
  "Retrieves the input code page used by the console associated with the calling process. ")

(define-external-function
    ("GetConsoleCursorInfo" console-cursor-info)
    (:stdcall kernel32)
  ((last-error bool) rv cursor-info)
  "Retrieves information about the size and visibility of the cursor for the specified console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (cursor-info (& console-cursor-info :out) :aux))

(define-enum (console-display-mode-flags
               (:base-type dword)
               (:conc-name console-))
  (:fullscreen 1)
  (:fullscreen-hardware 2))

(define-external-function
    ("GetConsoleDisplayMode" console-display-mode)
    (:stdcall kernel32)
  ((last-error bool) rv mode-flags)
  "Retrieves the display mode of the current console."
  (mode-flags (& console-display-mode-flags :out) :aux))

#-win2000
(define-external-function
    ("GetConsoleFontSize" (:camel-case))
    (:stdcall kernel32)
  ((last-error dword not-zero) rv
   (coord-from-dword rv))
  "Retrieves the size of the font used by the specified console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (font dword))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("GetConsoleHistoryInfo" console-history-info)
    (:stdcall kernel32)
  ((last-error bool) rv history-info)
  "Retrieves the history settings for the calling process's console."
  (history-info (& console-history-info :out) :aux))

(define-enum (console-mode
               (:base-type dword)
               (:conc-name nil))
  (:enable-echo-input #x0004)  
  (:enable-insert-mode #x0020)
  (:enable-line-io #x0002)
  (:enable-mouse-input #x0010)
  (:enable-processed-io #x0001)
  (:enable-quick-edit-mode #x0040)
  (:enable-window-input #x0008))

(define-external-function
    ("GetConsoleMode" console-mode)
    (:stdcall kernel32)
  ((last-error bool) rv mode)
  "Retrieves the current input mode of a console's input buffer or the current output mode of a console screen buffer."
  (console-handle handle :optional (std-handle :input-handle))
  (mode (& console-mode :out) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    (#+doors.unicode "GetConsoleOriginalTitleW"
     #-doors.unicode "GetConsoleOriginalTitleA"
                   get-console-original-title)
    (:stdcall kernel32)
  (dword rv (if (zerop rv)
              (last-error nil "")
              (subseq console-title 0 rv)))
  "Retrieves the original title for the current console window."
  (console-title (& tstring :out) :aux (make-string size))
  (size dword :optional 256))

(define-external-function
    ("GetConsoleOutputCP" console-output-code-page)
    (:stdcall kernel32)
  (uint)
  "Retrieves the output code page used by the console associated with the calling process. ")

#-win2000
(define-external-function
    ("GetConsoleProcessList" (:camel-case))
    (:stdcall kernel32)
  ((last-error dword not-zero) rv
   (if (<= rv process-count)
     (subseq list 0 dw)
     (external-function-call "GetConsoleProcessList"
       ((:stdcall kernel32)
        (dword rv (subseq new-list 0 rv))
        (new-list (& (~ dword) :out) :aux (make-list count))
        (count dword :aux rv)))))
  "Retrieves a list of the processes attached to the current console."
  (list (& (~ dword) :out) :aux '())
  (process-count dword :aux 0))

(define-external-function
    ("GetConsoleScreenBufferInfo" console-screen-buffer-info)
    (:stdcall kernel32)
  ((last-error boolean) rv info)
  "Retrieves information about the specified console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (info (& console-screen-buffer-info :out) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("GetConsoleScreenBufferInfoEx" console-screen-buffer-info-ex)
    (:stdcall kernel32)
  ((last-error bool) rv info)
  "Retrieves extended information about the specified console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (info (& console-screen-buffer-info-ex :out) :aux))

#-win2000
(define-external-function
    ("GetConsoleSelectionInfo" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv info)
  "Retrieves information about the current console selection."
  (info (& console-selection-info :out) :aux))

(define-external-function
    (#+doors.unicode "GetConsoleTitleW"
     #-doors.unicode "GetConsoleTitleA"
                   console-title)
    (:stdcall kernel32)
  ((last-error dword not-zero) rv (subseq title 0 rv))
  "Retrieves the title for the current console window."
  (title (& tstring :out) :aux (make-string size))
  (size dword :optional 256))

(define-external-function
    ("GetConsoleWindow" (:camel-case))
    (:stdcall kernel32)
  (handle)
  "Retrieves the window handle used by the console associated with the calling process.")

#-win2000
(define-external-function
    ("GetCurrentConsoleFont" current-console-font)
    (:stdcall kernel32)
  ((last-error bool) rv info)
  "Retrieves information about the current console font."
  (console-output handle :optional (std-handle :output-handle))
  (maximum-window-size bool :optional)
  (info (& console-font-info :out) :aux))

#-win2000
(define-external-function
    ("GetCurrentConsoleFontEx" current-console-font-ex)
    (:stdcall kernel32)
  ((last-error bool) rv info)
  "Retrieves extended information about the current console font."
  (console-output handle :optional (std-handle :output-handle))
  (maximum-window-size bool :optional)
  (info (& console-font-info-ex :out) :aux))

(define-external-function
    ("GetLargestConsoleWindowSize" (:camel-case))
    (:stdcall kernel32)
  (dword rv (if (zerop rv)
              (last-error)
              (coord-from-dword rv)))
  "Retrieves the size of the largest possible console window, based on the current font and the size of the display."
  (console-output handle :optional (std-handle :output-handle)))

(define-external-function
    ("GetNumberOfConsoleInputEvents" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Retrieves the number of unread input records in the console's input buffer."
  (console-input handle :optional (std-handle :input-handle))
  (n (& dword :out) :aux))

(define-external-function
    ("GetNumberOfConsoleMouseButtons" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Retrieves the number of buttons on the mouse used by the current console."
  (n (& dword :out) :aux))

(define-external-function
    (#+doors.unicode "PeekConsoleInputW"
     #-doors.unicode "PeekConsoleInputA"
                   peek-console-input)
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Reads data from the specified console input buffer without removing it from the buffer."
  (console-input handle :optional (std-handle :input-handle))
  (buffer (& (array input-record) :out))
  (length dword :optional (array-total-size buffer))
  (n (& dword :out) :aux))

(define-external-function
    (#+doors.unicode "ReadConsoleW"
     #-doors.unicode "ReadConsoleA"
                   read-console)
    (:stdcall kernel32)
  ((last-error bool) rv chars-readen)
  "Reads character input from the console input buffer and removes it from the buffer."
  (console-input handle :optional (std-handle :input-handle))
  (buffer (& tstring :out))
  (number-of-chars-to-read dword)
  (chars-readen (& dword :out) :aux)
  #+doors.unicode
  (input-control (& console-read-control :in t) :optional void)
  #-doors.unicode
  (input-control pointer :aux &0))

(define-external-function
    (#+doors.unicode "ReadConsoleInputW"
     #-doors.unicode "ReadConsoleInputA"
                   read-console-input)
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Reads data from a console input buffer and removes it from the buffer."
  (console-input handle :optional (std-handle :input-handle))
  (buffer (& (array input-record) :out))
  (length dword :optional (array-total-size buffer))
  (n (& dword :out) :aux))

(define-external-function
    (#+doors.unicode "ReadConsoleOutputW"
     #-doors.unicode "ReadConsoleOutputA"
                   read-console-output)
    (:stdcall kernel32)
  ((last-error bool) rv region)
  "Reads character and color attribute data from a rectangular block of character cells in a console screen buffer, and the function writes the data to a rectangular block at a specified location in the destination buffer."
  (console-output handle :optional (std-handle :output-handle))
  (buffer (& (array char-info) :out))
  (buffer-size-coord dword :optional (coord-to-dword
                                       (coord (array-dimension buffer 1)
                                              (array-dimension buffer 0))))
  (buffer-coord dword)
  (region (& small-rect :inout)))

(define-external-function
    ("ReadConsoleOutputAttribute" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Copies a specified number of character attributes from consecutive cells of a console screen buffer, beginning at a specified location."
  (console-output handle :optional (std-handle :output-handle))
  (attrs (& (array word) :out))
  (length dword :optional (array-total-size attrs))
  (read-coord dword)
  (n (& dword :out) :aux))

(define-external-function
    (#+doors.unicode "ReadConsoleOutputCharacterW"
     #-doors.unicode "ReadConsoleOutputCharacterA"
                   read-console-output-character)
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Copies a number of characters from consecutive cells of a console screen buffer, beginning at a specified location."
  (console-output handle :optional (std-handle :output-handle))
  (buffer (& tstring :out))
  (length dword :optional (length buffer))
  (read-coord dword)
  (n (& dword :out) :aux))

(define-external-function
    (#+doors.unicode "ScrollConsoleScreenBufferW"
     #-doors.unicode "ScrollConsoleScreenBufferA"
                   scroll-console-screen-buffer)
    (:stdcall kernel32)
  ((last-error bool))
  "Moves a block of data in a screen buffer. "
  (console-output handle :optional (std-handle :output-handle))
  (scroll-rectangle (& small-rect))
  (clip-rectangle (& small-rect :in t) :optional void)
  (destination-origin dword)
  (fill (& char-info)))

(define-external-function
    ("SetConsoleActiveScreenBuffer" (setf console-active-screen-buffer))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the specified screen buffer to be the currently displayed console screen buffer."
  (console-output handle))

(define-external-function
    ("SetConsoleCP" (setf console-input-code-page))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the input code page used by the console associated with the calling process."
  (code-page-id uint))

(define-external-function
    ("SetConsoleCtrlHandler" (setf console-ctrl-handler))
    (:stdcall kernel32)
  ((last-error bool))
  "Adds or removes an application-defined callback from the list of handler functions for the calling process."
  (handler-routine pointer)
  (add bool :optional t))

(define-external-function
    ("SetConsoleOutputCP" (setf console-output-code-page))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the output code page used by the console associated with the calling process. "
  (code-page-id uint))

(define-external-function
    ("SetConsoleMode" (setf console-mode))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the input mode of a console's input buffer or the output mode of a console screen buffer."
  (console-handle handle :optional (std-handle :input-handle))
  (mode console-mode))

(define-external-function
    ("SetConsoleCursorInfo" (setf console-cursor-info))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the size and visibility of the cursor for the specified console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (cursor-info (& console-cursor-info)))

(define-external-function
    ("SetConsoleCursorPosition" (setf console-cursor-position))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the cursor position in the specified console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (coord dword))

#-win2000
(define-external-function
    ("SetConsoleDisplayMode" (setf console-display-mode))
    (:stdcall kernel32)
  ((last-error bool) rv new-screen-buffer-dims)
  (console-output handle :optional (std-handle :output-handle))
  (flags console-display-mode)
  (new-screen-buffer-dims (& coord :out) :aux))

#-(or win2000 winxp winxp64 winhomeserver winserver2003)
(define-external-function
    ("SetConsoleHistoryInfo" (setf console-history-info))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the history settings for the calling process's console."
  (history-info (& console-history-info)))

#-(or win2000 winxp winxp64 winhomeserver winserver2003)
(define-external-function
    ("SetConsoleScreenBufferInfoEx" (setf console-screen-buffer-info-ex))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets extended information about the specified console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (info (& console-screen-buffer-info-ex)))

(define-external-function
    ("SetConsoleScreenBufferSize" (setf console-screen-buffer-size))
    (:stdcall kernel32)
  ((last-error bool))
  "Changes the size of the specified console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (size-coord dword))

(define-external-function
    ("SetConsoleTextAttribute" (setf console-text-attribute))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the attributes of characters written to the console screen buffer."
  (console-output handle :optional (std-handle :output-handle))
  (attributes char-attributes))

(define-external-function
    ("SetStdHandle" (setf std-handle))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the handle for the specified standard device (standard input, standard output, or standard error)."
  (handle-type std-handle :optional 0)
  (handle handle))

(define-external-function
    (#+doors.unicode "SetConsoleTitleW"
     #-doors.unicode "SetConsoleTitleA"
                   (setf console-title))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the title for the current console window."
  (console-title (& tstring)))

(define-external-function
    ("SetConsoleWindowInfo" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets the current size and position of a console screen buffer's window."
  (console-output handle :optional (std-handle :output-handle))  
  (absolute boolean)
  (console-window (& small-rect)))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("SetCurrentConsoleFontEx" (setf current-console-font-ex))
    (:stdcall kernel32)
  ((last-error bool))
  "Sets extended information about the current console font."
  (console-output handle :optional (std-handle :output-handle))
  (maximum-window bool :optional)
  (info (& console-current-font-ex)))

(define-external-function
    (#+doors.unicode "WriteConsoleW"
     #-doors.unicode "WriteConsoleA"
                   write-console)
    (:stdcall kernel32)
  ((last-error bool) rv chars-written)
  "Writes a character string to a console screen buffer beginning at the current cursor location."
  (console-output handle :optional (std-handle :output-handle))
  (buffer (& tstring))
  (chars-to-write dword :optional (length buffer))
  (chars-written (& dword :out) :aux)
  (reserved pointer :aux &0))

(define-external-function
    (#+doors.unicode "WriteConsoleInputW"
     #-doors.unicode "WriteConsoleInputA"
                   write-console-input)
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Writes data directly to the console input buffer."
  (console-input handle :optional (std-handle :input-handle))
  (buffer (& (array input-record)))
  (length dword :optional (array-total-size buffer))
  (n (& dword :out) :aux))

(define-external-function
    (#+doors.unicode "WriteConsoleOutputW"
     #-doors.unicode "WriteConsoleOutputA"
                   write-console-output)
    (:stdcall kernel32)
  ((last-error bool) rv region)
  "Writes character and color attribute data to a specified rectangular block of character cells in a console screen buffer. T"
  (console-output handle :optional (std-handle :output-handle))
  (buffer (& (array char-info)))
  (buffer-size-coord dword :optional (coord-to-dword
                                       (coord (array-dimension buffer 1)
                                              (array-dimension buffer 0))))
  (buffer-coord dword :optional 0)
  (region (& small-rect :inout)))

(define-external-function
    ("WriteConsoleOutputAttribute" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Copies a number of character attributes to consecutive cells of a console screen buffer, beginning at a specified location."
  (console-output handle :optional (std-handle :output-handle))
  (attributes (& (array word)))
  (length dword :optional (array-total-size attributes))
  (write-coord dword)
  (n (& dword :out) :aux))

(define-external-function
    (#+doors.unicode "WriteConsoleOutputCharacterW"
     #-doors.unicode "WriteConsoleOutputCharacterA"
                   write-console-output-character)
    (:stdcall kernel32)
  ((last-error bool) rv n)
  "Copies a number of characters to consecutive cells of a console screen buffer, beginning at a specified location."
  (console-output handle :optional (std-handle :output-handle))
  (characters (& tstring))
  (length dword :optional (length characters))
  (write-coord dword)
  (n (& dword :out) :aux))
