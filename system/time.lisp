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

(declaim (inline make-file-time make-file-time*
                 low-date-time high-date-time quad-date-time
                 (setf low-date-time) (setf high-date-time)
                 (setf quad-date-time)))
(define-struct (file-time
                 (:conc-name nil)
                 (:constructor make-file-time (&optional low-date-time high-date-time))
                 (:constructor make-file-time* (&optional (quad-date-time 0)
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
                 (:include time-zone-information)
                 (:conc-name time-zone-))
  (key-name (wstring 128))
  (dynamic-daylight-time-disabled-p bool))

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

(define-external-function
    ("DosDateTimeToFileTime" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv ftime)
  "Converts MS-DOS date and time values to a file time."
  (fat-date word)
  (fat-time word)
  (ftime (& file-time :out) :aux))

(define-external-function
    ("FileTimeToDosDateTime" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv (values fat-date fat-time))
  "Converts a file time to MS-DOS date and time values."
  (file-time (& file-time))
  (fat-date (& word :out) :aux)
  (fat-time (& word :out) :aux))

(define-external-function
    ("FileTimeToLocalFileTime" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv local-time)
  "Converts a file time to a local file time."
  (file-time (& file-time))
  (local-time (& file-time :out) :aux))

(define-external-function
    ("FileTimeToSystemTime" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv systime)
  "Converts a file time to system time format. System time is based on Coordinated Universal Time (UTC)."
  (file-time (& file-time))
  (systime (& system-time :out) :aux))

(define-enum (time-zone-id
               (:base-type dword))
  :unknown
  :standard
  :daylight
  (:invalid #xFFFFFFFF))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("GetDynamicTimeZoneInformation"
      dynamic-time-zone-information)
    (:stdcall kernel32)
  (time-zone-id rv (if (eq rv :invalid)
                     (invoke-last-error)
                     (values info rv)))
  "Retrieves the current time zone and dynamic daylight saving time settings. "
  (info (& dynamic-time-zone-information :out) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-symbol-macro dynamic-time-zone-information
    (dynamic-time-zone-information))

(define-external-function
    ("GetFileTime" file-time)
    (:stdcall kernel32)
  ((last-error bool) rv (values creation-time
                                last-access-time
                                last-write-time))
  "Retrieves the date and time that a file or directory was created, last accessed, and last modified."
  (file handle)
  (creation-time (& file-time :out) :aux)
  (last-access-time (& file-time :out) :aux)
  (last-write-time (& file-time :out) :aux))

(define-external-function
    ("GetLocalTime" local-time)
    (:stdcall kernel32)
  (void rv system-time)
  "Retrieves the current local date and time."
  (system-time (& system-time :out) :aux))

(define-symbol-macro local-time (local-time))

(define-external-function
    ("GetSystemTimeAsFileTime" system-time-as-file-time)
    (:stdcall kernel32)
  (void rv file-time)
  "Retrieves the current system date and time. The information is in Coordinated Universal Time (UTC) format."
  (file-time (& file-time :out) :aux))

(define-symbol-macro system-time-as-file-time (system-time-as-file-time))

#-win2000
(define-external-function
    ("GetSystemTimes" system-times)
    (:stdcall kernel32)
  ((last-error bool) rv (values idle-time kernel-time user-time))
  "Retrieves system timing information."
  (idle-time (& file-time :out) :aux)
  (kernel-time (& file-time :out) :aux)
  (user-time (& file-time :out) :aux))

#-win2000
(define-symbol-macro system-times (system-times))

(define-external-function
    ("GetTickCount" tick-count)
    (:stdcall kernel32)
  (dword)
  "Retrieves the number of milliseconds that have elapsed since the system was started, up to 49.7 days.")

(define-symbol-macro tick-count (tick-count))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("GetTickCount64" tick-count*)
    (:stdcall kernel32)
  (ullong)
  "Retrieves the number of milliseconds that have elapsed since the system was started.")

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-symbol-macro tick-count* (tick-count*))

(define-external-function
    ("GetTimeZoneInformation" time-zone-information)
    (:stdcall kernel32)
  (time-zone-id rv (if (eq rv :invalid)
                     (invoke-last-error)
                     (values info rv)))
  "Retrieves the current time zone settings."
  (info (& time-zone-information :out) :aux))

(define-symbol-macro time-zone-information (time-zone-information))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("GetTimeZoneInformationForYear"
      time-zone-information-for-year)
    (:stdcall kernel32)
  ((last-error bool) rv info)
  "Retrieves the time zone settings for the specified year and time zone. "
  (year ushort)
  (dtzi (& dynamic-time-zone-information :in t) :optional void)
  (info (& time-zone-information :out) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-symbol-macro time-zone-information-for-year (time-zone-information-for-year))

(define-external-function
    ("LocalFileTimeToFileTime" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv file-time)
  "Converts a local file time to a file time based on the Coordinated Universal Time (UTC)."
  (local-file-time (& file-time))
  (file-time (& file-time :out) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver winvista winserver2008)
(define-external-function
    ("QueryUnbiasedInterruptTime" unbiased-interrupt-time)
    (:stdcall kernel32)
  ((last-error bool) rv time)
  "Gets the current unbiased interrupt-time count. "
  (time (& ullong) :aux))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver winvista winserver2008)
(define-symbol-macro unbiased-interrupt-time (unbiased-interrupt-time))

#-(or win2000 winxp winxp64 winserver2003 winhomeserver)
(define-external-function
    ("SetDynamicTimeZoneInformation"
      (setf dynamic-time-zone-information))
    (:stdcall kernel32)
  ((last-error bool) rv time-zone-information)
  "Sets the current time zone and dynamic daylight saving time settings. "
  (time-zone-information (& dynamic-time-zone-information)))

(define-external-function
    ("SetFileTime" (setf file-time))
    (:stdcall kernel32)
  ((last-error bool) rv (values creation-time last-access-time last-write-time))
  "Sets the date and time that the specified file or directory was created, last accessed, or last modified."
  (file handle)
  (creation-time (& file-time :in t) :key)
  (last-access-time (& file-time :in t) :key)
  (last-write-time (& file-time :in t) :key))

(define-external-function
    ("SetLocalTime" (setf local-time))
    (:stdcall kernel32)
  ((last-error bool) rv system-time)
  "Sets the current local time and date."
  (system-time (& system-time)))

(define-external-function
    ("SetSystemTime" (setf system-time))
    (:stdcall kernel32)
  ((last-error bool) rv system-time)
  "Sets the current system time and date."
  (system-time (& system-time)))

(define-external-function
    ("SetSystemTimeAdjustment" (setf system-time-adjustment))
    (:stdcall kernel32)
  ((last-error bool) rv (values adjustment adjustment-disabled))
  "Enables or disables periodic time adjustments to the system's time-of-day clock. "
  (adjustment dword)
  (adjustment-disabled boolean :optional))

(define-external-function
    ("SetTimeZoneInformation" (setf time-zone-information))
    (:stdcall kernel32)
  ((last-error bool) rv time-zone-information)
  "Sets the current time zone settings."
  (time-zone-information (& time-zone-information)))

(define-external-function
    ("SystemTimeToFileTime" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv file-time)
  "Converts a system time to file time format. System time is based on Coordinated Universal Time (UTC)."
  (system-time (& system-time))
  (file-time (& file-time :out) :aux))

(define-external-function
    ("SystemTimeToTzSpecificLocalTime" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv local-time)
  "Converts a time in Coordinated Universal Time (UTC) to a specified time zone's corresponding local time."
  (time-zone-info (& time-zone-information :in t) :optional void)
  (universal-time (& system-time))
  (local-time (& system-time :out) :aux))

#-win2000
(define-external-function
    ("TzSpecificLocalTimeToSystemTime" (:camel-case))
    (:stdcall kernel32)
  ((last-error bool) rv universal-time)
  "Converts a time in Coordinated Universal Time (UTC) to a specified time zone's corresponding local time."
  (time-zone-info (& time-zone-information :in t) :optional void)
  (local-time (& file-time))
  (universal-time (& system-time :out) :aux))
