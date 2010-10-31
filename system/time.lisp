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

(declaim (inline file-time file-time* make-file-time
                 low-date-time high-date-time quad-date-time
                 (setf low-date-time) (setf high-date-time)
                 (setf quad-date-time)))
(define-struct (file-time
                 (:conc-name nil)
                 (:constructor make-file-time)
                 (:constructor file-time (low-date-time high-date-time))
                 (:constructor file-time* (quad-date-time
                                            &aux
                                            (low-date-time
                                              (ldb (byte 32 0) quad-date-time))
                                            (high-date-time
                                              (ldb (byte 32 32) quad-date-time)))))
    "Contains a 64-bit value representing the number of 100-nanosecond intervals since January 1, 1601 (UTC)."
  (low-date-time dword)
  (high-date-time dword))

(defun quad-date-time (file-time)
  (declare (type file-time file-time))
  (the qword
       (logior (low-date-time file-time)
               (ash (high-date-time file-time) 32))))

(defun (setf quad-date-time) (new-value file-time)
  (declare (type qword new-value)
           (type file-time file-time))
  (setf (low-date-time file-time) (ldb (byte 32 0) new-value)
        (high-date-time file-time) (ldb (byte 32 32) new-value))
  new-value)

(define-struct (system-time
                 (:constructor make-system-time))
    "Specifies a date and time, using individual members for the month, day, year, weekday, hour, minute, second, and millisecond. "
  (year word)
  (month (enum (:base-type word)
               (:january 1)
               :february
               :march
               :april
               :may
               :june
               :july
               :august
               :september
               :october
               :november
               :december))
  (day-of-week (enum (:base-type word)
                     :sunday
                     :monday
                     :tuesday
                     :wednesday
                     :thursday
                     :friday
                     :saturday))
  (day word)
  (hour word)
  (minute word)
  (second word)
  (millisecond word))

(define-struct (time-zone-information
                 (:conc-name time-zone-))
  (bias long)
  (standard-name (wstring 32))
  (standard-date system-time)
  (standard-bias long)
  (daylight-name (wstring 32))
  (daylight-date system-time)
  (daylight-bias long))

(define-struct (dynamic-time-zone-information
                 (:conc-name dynamic-time-zone-))
  (bias long)
  (standard-name (wstring 32))
  (standard-date system-time)
  (standard-bias long)
  (daylight-name (wstring 32))
  (daylight-date system-time)
  (daylight-bias long)
  (key-name (wstring 128))
  (dynamic-daylight-time-disabled bool))

(define-external-function
    ("GetSystemTime" system-time)
    (:stdcall kernel32)
  (void rv time)
  "Retrieves the current system date and time."
  (time (& system-time :out) :aux))

(define-symbol-macro system-time (system-time))

(define-external-function
    ("GetSystemTimeAdjustment" system-time-adjustment)
    (:stdcall kernel32)
  ((last-error bool) rv (values adjustment increment adjustment-disabled))
  "Determines whether the system is applying periodic time adjustments to its time-of-day clock at each clock interrupt, along with the value and period of any such adjustments."
  (adjustment (& dword :out) :aux)
  (increment (& dword :out) :aux)
  (adjustment-disabled (& bool :out) :aux))

(define-symbol-macro system-time-adjustment (system-time-adjustment))

(define-external-function
    ("CompareFileTime" (:camel-case))
    (:stdcall kernel32)
  (long)
  "Compares two file times."
  (file-time-1 (& file-time))
  (file-time-2 (& file-time)))
