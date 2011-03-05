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

(define-enum (system-metrics
               (:base-type int)
               (:conc-name sm-))
  (:arrange 56)
  (:clean-boot 67)
  (:monitors 80)
  (:mouse-buttons 43)
  (:x-border 5)
  (:x-cursor 13)
  (:x-dlg-frame 7)
  (:x-double-clk 36)
  (:x-drag 68)
  (:x-edge 45)
  (:x-fixed-frame 7)
  (:x-focus-border 83)
  (:x-frame 32)
  (:x-fullscreen 16)
  (:x-hscroll 21)
  (:x-hthumb 10)
  (:x-icon 11)
  (:x-icon-spacing 38)
  (:x-maximized 61)
  (:x-max-track 59)
  (:x-menu-check 71)
  (:x-menu-size 54)
  (:x-min 28)
  (:x-minimized 57)
  (:x-min-spacing 47)
  (:x-min-track 34)
  (:x-padded-border 92)
  (:x-screen 0)
  (:x-size 30)
  (:x-size-frame 32)
  (:x-small-icon 49)
  (:x-small-size 52)
  (:cx-virtual-screen 78)
  (:x-vscroll 2)
  (:y-border 6)
  (:y-caption 4)
  (:y-cursor 14)
  (:y-dlg-frame 8)
  (:y-double-clk 37)
  (:y-drag 69)
  (:y-edge 46)
  (:y-fixed-frame 8)
  (:y-focus-border 84)
  (:y-frame 33)
  (:y-fullscreen 17)
  (:y-hscroll 3)
  (:y-icon 12)
  (:y-icon-spacing 39)
  (:y-kanji-window 18)
  (:y-maximized 62)
  (:y-max-track 60)
  (:y-menu 15)
  (:y-menu-check 72)
  (:y-menu-size 55)
  (:y-min 29)
  (:y-minimized 58)
  (:y-min-spacing 48)
  (:y-min-track 35)
  (:y-screen 1)
  (:y-size 31)
  (:y-size-frame 33)
  (:y-small-caption 51)
  (:y-small-icon 50)
  (:y-small-size 53)
  (:cy-virtual-screen 79)
  (:y-vscroll 20)
  (:y-vthumb 9)
  (:dbcs-enabled 42)
  (:debug 22)
  (:digitizer 94)
  (:imm-enabled 82)
  (:maximum-touches 95)
  (:media-center 87)
  (:menu-rop-alignment 40)
  (:mid-east-enabled 74)
  (:mouse-present 19)
  (:mouse-horizontal-wheel-present 91)
  (:mouse-wheel-present 75)
  (:network 63)
  (:pen-windows 41)
  (:remote-control #x2001)
  (:remote-session #x1000)
  (:same-display-format 81)
  (:secure 44)
  (:server-r2 89)
  (:show-sounds 70)
  (:shutting-down #x2000)
  (:slow-machine 73)
  (:starter 88)
  (:swap-button 23)
  (:tablet-pc 86)
  (:x-virtual-screen 76)
  (:y-virtual-screen 77))

(define-external-function
    ("GetSystemMetrics" system-metrics)
    (:stdcall user32)
  (int)
  "Retrieves the specified system metric or system configuration setting."
  (index system-metrics))

