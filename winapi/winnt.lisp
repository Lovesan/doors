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

(declaim (inline large-integer
                 make-large-integer
                 copy-large-integer
                 large-integer-low-part
                 large-integer-high-part
                 large-integer-quad-part
                 (setf large-integer-low-part)
                 (setf large-integer-high-part)
                 (setf large-integer-quad-part)))
(define-struct (large-integer
                 (:constructor
                   large-integer (quad-part)))
  (quad-part int64))

(defun make-large-integer (low-part high-part)
  (declare (type int32 high-part)
           (type uint32 low-part))
  (large-integer (make-long-long low-part high-part)))

(defun large-integer-low-part (large-integer)
  (declare (type large-integer large-integer))
  (low-dword (large-integer-quad-part large-integer)))
(defun (setf large-integer-low-part) (new-value large-integer)
  (declare (type large-integer large-integer))
  (setf (ldb (byte 32 0)
             (large-integer-quad-part large-integer))
        new-value))

(defun large-integer-high-part (large-integer)
  (declare (type large-integer large-integer))
  (make-long
    (high-dword (large-integer-quad-part large-integer))
    #xFFFFFFFF))

(defun (setf large-integer-high-part) (new-value large-integer)
  (declare (type large-integer large-integer))
  (setf (ldb (byte 32 32)
             (large-integer-quad-part large-integer))
        new-value))

(declaim (inline ularge-integer
                 make-ularge-integer
                 copy-large-integer
                 ularge-integer-low-part
                 ularge-integer-high-part
                 ularge-integer-quad-part
                 (setf ularge-integer-low-part)
                 (setf ularge-integer-high-part)
                 (setf ularge-integer-quad-part)))
(define-struct (ularge-integer
                 (:constructor
                   ularge-integer (quad-part)))
  (quad-part uint64))

(defun make-ularge-integer (low-part high-part)
  (declare (type uint32 high-part)
           (type uint32 low-part))
  (ularge-integer (make-qword low-part high-part)))

(defun ularge-integer-low-part (ularge-integer)
  (declare (type ularge-integer ularge-integer))
  (low-dword (ularge-integer-quad-part ularge-integer)))
(defun (setf ularge-integer-low-part) (new-value ularge-integer)
  (declare (type ularge-integer ularge-integer))
  (setf (ldb (byte 32 0)
             (ularge-integer-quad-part ularge-integer))
        new-value))

(defun ularge-integer-high-part (ularge-integer)
  (declare (type ularge-integer ularge-integer))
  (high-dword (ularge-integer-quad-part ularge-integer)))
(defun (setf ularge-integer-high-part) (new-value ularge-integer)
  (declare (type ularge-integer ularge-integer))
  (setf (ldb (byte 32 32)
             (ularge-integer-quad-part ularge-integer))
        new-value))

(declaim (inline luid
                 copy-luid
                 luid-quad-part
                 luid-low-part
                 luid-high-part
                 (setf luid-quad-part)
                 (setf luid-low-part)
                 (setf luid-high-part)))
(define-struct (luid
                 (:include large-integer)
                 (:constructor luid (low-part high-part
                                     &aux (quad-part (make-long-long
                                                       low-part
                                                       high-part))))))

(defun luid-low-part (luid)
  (declare (type luid luid))
  (low-dword (luid-quad-part luid)))

(defun (setf luid-low-part) (new-value luid)
  (declare (type uint32 new-value) (type luid luid))
  (setf (ldb (byte 32 0) (luid-quad-part luid))
        new-value))

(defun luid-high-part (luid)
  (declare (type luid luid))
  (make-long
    (high-dword (luid-quad-part luid))
    #xFFFFFFFF))

(defun (setf luid-high-part) (new-value luid)
  (declare (type int32 new-value) (type luid luid))
  (setf (ldb (byte 32 32) (luid-quad-part luid))
        new-value))

(defconstant unicode-string-max-bytes 65534)
(defconstant unicode-string-max-chars 32767)

(define-struct (list-entry
             (:constructor list-entry (&key flink blink)))
  (flink pointer)
  (blink pointer))

(define-struct (single-list-entry
             (:constructor single-list-entry (next)))
  (next pointer))

(define-enum (LANG)
  (:NEUTRAL                     #x00)
  (:INVARIANT                   #x7f)
  (:AFRIKAANS                   #x36)
  (:ALBANIAN                    #x1c)
  (:ARABIC                      #x01)
  (:ARMENIAN                    #x2b)
  (:ASSAMESE                    #x4d)
  (:AZERI                       #x2c)
  (:BASQUE                      #x2d)
  (:BELARUSIAN                  #x23)
  (:BENGALI                     #x45)
  (:BULGARIAN                   #x02)
  (:CATALAN                     #x03)
  (:CHINESE                     #x04)
  (:CROATIAN                    #x1a)
  (:CZECH                       #x05)
  (:DANISH                      #x06)
  (:DIVEHI                      #x65)
  (:DUTCH                       #x13)
  (:ENGLISH                     #x09)
  (:ESTONIAN                    #x25)
  (:FAEROESE                    #x38)
  (:FARSI                       #x29)
  (:FINNISH                     #x0b)
  (:FRENCH                      #x0c)
  (:GALICIAN                    #x56)
  (:GEORGIAN                    #x37)
  (:GERMAN                      #x07)
  (:GREEK                       #x08)
  (:GUJARATI                    #x47)
  (:HEBREW                      #x0d)
  (:HINDI                       #x39)
  (:HUNGARIAN                   #x0e)
  (:ICELANDIC                   #x0f)
  (:INDONESIAN                  #x21)
  (:ITALIAN                     #x10)
  (:JAPANESE                    #x11)
  (:KANNADA                     #x4b)
  (:KASHMIRI                    #x60)
  (:KAZAK                       #x3f)
  (:KONKANI                     #x57)
  (:KOREAN                      #x12)
  (:KYRGYZ                      #x40)
  (:LATVIAN                     #x26)
  (:LITHUANIAN                  #x27)
  (:MACEDONIAN                  #x2f)
  (:MALAY                       #x3e)
  (:MALAYALAM                   #x4c)
  (:MANIPURI                    #x58)
  (:MARATHI                     #x4e)
  (:MONGOLIAN                   #x50)
  (:NEPALI                      #x61)
  (:NORWEGIAN                   #x14)
  (:ORIYA                       #x48)
  (:POLISH                      #x15)
  (:PORTUGUESE                  #x16)
  (:PUNJABI                     #x46)
  (:ROMANIAN                    #x18)
  (:RUSSIAN                     #x19)
  (:SANSKRIT                    #x4f)
  (:SERBIAN                     #x1a)
  (:SINDHI                      #x59)
  (:SLOVAK                      #x1b)
  (:SLOVENIAN                   #x24)
  (:SPANISH                     #x0a)
  (:SWAHILI                     #x41)
  (:SWEDISH                     #x1d)
  (:SYRIAC                      #x5a)
  (:TAMIL                       #x49)
  (:TATAR                       #x44)
  (:TELUGU                      #x4a)
  (:THAI                        #x1e)
  (:TURKISH                     #x1f)
  (:UKRAINIAN                   #x22)
  (:URDU                        #x20)
  (:UZBEK                       #x43)
  (:VIETNAMESE                  #x2a))

(define-enum (sublang)
  (:NEUTRAL                  #x00)    ;; language neutral
  (:DEFAULT                  #x01)    ;; user default
  (:SYS-DEFAULT              #x02)    ;; system default
  (:ARABIC-SAUDI-ARABIA      #x01)    ;; Arabic (Saudi Arabia)
  (:ARABIC-IRAQ              #x02)    ;; Arabic (Iraq)
  (:ARABIC-EGYPT             #x03)    ;; Arabic (Egypt)
  (:ARABIC-LIBYA             #x04)    ;; Arabic (Libya)
  (:ARABIC-ALGERIA           #x05)    ;; Arabic (Algeria)
  (:ARABIC-MOROCCO           #x06)    ;; Arabic (Morocco)
  (:ARABIC-TUNISIA           #x07)    ;; Arabic (Tunisia)
  (:ARABIC-OMAN              #x08)    ;; Arabic (Oman)
  (:ARABIC-YEMEN             #x09)    ;; Arabic (Yemen)
  (:ARABIC-SYRIA             #x0a)    ;; Arabic (Syria)
  (:ARABIC-JORDAN            #x0b)    ;; Arabic (Jordan)
  (:ARABIC-LEBANON           #x0c)    ;; Arabic (Lebanon)
  (:ARABIC-KUWAIT            #x0d)    ;; Arabic (Kuwait)
  (:ARABIC-UAE               #x0e)    ;; Arabic (U.A.E)
  (:ARABIC-BAHRAIN           #x0f)    ;; Arabic (Bahrain)
  (:ARABIC-QATAR             #x10)    ;; Arabic (Qatar)
  (:AZERI-LATIN              #x01)    ;; Azeri (Latin)
  (:AZERI-CYRILLIC           #x02)    ;; Azeri (Cyrillic)
  (:CHINESE-TRADITIONAL      #x01)    ;; Chinese (Taiwan)
  (:CHINESE-SIMPLIFIED       #x02)    ;; Chinese (PR China)
  (:CHINESE-HONGKONG         #x03)    ;; Chinese (Hong Kong S.A.R., P.R.C.)
  (:CHINESE-SINGAPORE        #x04)    ;; Chinese (Singapore)
  (:CHINESE-MACAU            #x05)    ;; Chinese (Macau S.A.R.)
  (:DUTCH                    #x01)    ;; Dutch
  (:DUTCH-BELGIAN            #x02)    ;; Dutch (Belgian)
  (:ENGLISH-US               #x01)    ;; English (USA)
  (:ENGLISH-UK               #x02)    ;; English (UK)
  (:ENGLISH-AUS              #x03)    ;; English (Australian)
  (:ENGLISH-CAN              #x04)    ;; English (Canadian)
  (:ENGLISH-NZ               #x05)    ;; English (New Zealand)
  (:ENGLISH-EIRE             #x06)    ;; English (Irish)
  (:ENGLISH-SOUTH-AFRICA     #x07)    ;; English (South Africa)
  (:ENGLISH-JAMAICA          #x08)    ;; English (Jamaica)
  (:ENGLISH-CARIBBEAN        #x09)    ;; English (Caribbean)
  (:ENGLISH-BELIZE           #x0a)    ;; English (Belize)
  (:ENGLISH-TRINIDAD         #x0b)    ;; English (Trinidad)
  (:ENGLISH-ZIMBABWE         #x0c)    ;; English (Zimbabwe)
  (:ENGLISH-PHILIPPINES      #x0d)    ;; English (Philippines)
  (:FRENCH                   #x01)    ;; French
  (:FRENCH-BELGIAN           #x02)    ;; French (Belgian)
  (:FRENCH-CANADIAN          #x03)    ;; French (Canadian)
  (:FRENCH-SWISS             #x04)    ;; French (Swiss)
  (:FRENCH-LUXEMBOURG        #x05)    ;; French (Luxembourg)
  (:FRENCH-MONACO            #x06)    ;; French (Monaco)
  (:GERMAN                   #x01)    ;; German
  (:GERMAN-SWISS             #x02)    ;; German (Swiss)
  (:GERMAN-AUSTRIAN          #x03)    ;; German (Austrian)
  (:GERMAN-LUXEMBOURG        #x04)    ;; German (Luxembourg)
  (:GERMAN-LIECHTENSTEIN     #x05)    ;; German (Liechtenstein)
  (:ITALIAN                  #x01)    ;; Italian
  (:ITALIAN-SWISS            #x02)    ;; Italian (Swiss)
#-win2000
  (:KASHMIRI-SASIA           #x02)    ;; Kashmiri (South Asia)
  (:KASHMIRI-INDIA           #x02)    ;; For app compatibility only
  (:KOREAN                   #x01)    ;; Korean (Extended Wansung)
  (:LITHUANIAN               #x01)    ;; Lithuanian
  (:MALAY-MALAYSIA           #x01)    ;; Malay (Malaysia)
  (:MALAY-BRUNEI-DARUSSALAM  #x02)    ;; Malay (Brunei Darussalam)
  (:NEPALI-INDIA             #x02)    ;; Nepali (India)
  (:NORWEGIAN-BOKMAL         #x01)    ;; Norwegian (Bokmal)
  (:NORWEGIAN-NYNORSK        #x02)    ;; Norwegian (Nynorsk)
  (:PORTUGUESE               #x02)    ;; Portuguese
  (:PORTUGUESE-BRAZILIAN     #x01)    ;; Portuguese (Brazilian)
  (:SERBIAN-LATIN            #x02)    ;; Serbian (Latin)
  (:SERBIAN-CYRILLIC         #x03)    ;; Serbian (Cyrillic)
  (:SPANISH                  #x01)    ;; Spanish (Castilian)
  (:SPANISH-MEXICAN          #x02)    ;; Spanish (Mexican)
  (:SPANISH-MODERN           #x03)    ;; Spanish (Spain)
  (:SPANISH-GUATEMALA        #x04)    ;; Spanish (Guatemala)
  (:SPANISH-COSTA-RICA       #x05)    ;; Spanish (Costa Rica)
  (:SPANISH-PANAMA           #x06)    ;; Spanish (Panama)
  (:SPANISH-DOMINICAN-REPUBLIC #x07)  ;; Spanish (Dominican Republic)
  (:SPANISH-VENEZUELA        #x08)    ;; Spanish (Venezuela)
  (:SPANISH-COLOMBIA         #x09)    ;; Spanish (Colombia)
  (:SPANISH-PERU             #x0a)    ;; Spanish (Peru)
  (:SPANISH-ARGENTINA        #x0b)    ;; Spanish (Argentina)
  (:SPANISH-ECUADOR          #x0c)    ;; Spanish (Ecuador)
  (:SPANISH-CHILE            #x0d)    ;; Spanish (Chile)
  (:SPANISH-URUGUAY          #x0e)    ;; Spanish (Uruguay)
  (:SPANISH-PARAGUAY         #x0f)    ;; Spanish (Paraguay)
  (:SPANISH-BOLIVIA          #x10)    ;; Spanish (Bolivia)
  (:SPANISH-EL-SALVADOR      #x11)    ;; Spanish (El Salvador)
  (:SPANISH-HONDURAS         #x12)    ;; Spanish (Honduras)
  (:SPANISH-NICARAGUA        #x13)    ;; Spanish (Nicaragua)
  (:SPANISH-PUERTO-RICO      #x14)    ;; Spanish (Puerto Rico)
  (:SWEDISH                  #x01)    ;; Swedish
  (:SWEDISH-FINLAND          #x02)    ;; Swedish (Finland)
  (:URDU-PAKISTAN            #x01)    ;; Urdu (Pakistan)
  (:URDU-INDIA               #x02)    ;; Urdu (India)
  (:UZBEK-LATIN              #x01)    ;; Uzbek (Latin)
  (:UZBEK-CYRILLIC           #x02))   ;; Uzbek (Cyrillic)

(define-enum (lang-sort (:conc-name sort-))
  (:default        0)
  (:japanese-xjis  0)
  (:japanese-unicode 1)
  (:chinese-big5 0)
  (:chinese-prcp 0)
  (:chinese-unicode 1)
  (:chinese-prc 2)
  (:chinese-bopomofo 3)
  (:korean-ksc 0)
  (:korean-unicode 1)
  (:german-phone-book 1)
  (:hungarian-default 0)
  (:hungarian-technical 1)
  (:georgian-traditional 0)
  (:georgian-modern 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
(declaim (inline make-lang-id))
(defun make-lang-id (primary sublang)
  (logior (ash (logand #xFFFF (convert sublang 'sublang))
               10)
          (logand #xFFFF (convert primary 'lang))))

(declaim (inline primary-lang-id))
(defun primary-lang-id (lang-id)
  (translate (logand lang-id #x3FF) 'lang))

(declaim (inline sublang-id))
(defun sublang-id (lang-id)
  (translate (ash (logand lang-id #xFFFF) -10) 'sublang))

(defconstant nls-valid-locale-mask #x000FFFFF)

(declaim (inline make-locale-id))
(defun make-locale-id (lang-id sort-id)
  (logand nls-valid-locale-mask
          (logior (ash (logand #xFFFF (convert sort-id 'lang-sort)) 16)
                  (logand #xFFFF lang-id))))

(declaim (inline make-sort-locale-id))
(defun make-sort-locale-id (lang-id sort-id sort-version)
  (logand nls-valid-locale-mask
          (logior (make-locale-id lang-id sort-id)
                  (ash (logand #xFFFF sort-version) 20))))

(declaim (inline lang-id-from-locale-id))
(defun lang-id-from-locale-id (locale-id)
  (logand #xFFFF locale-id))

(declaim (inline sort-id-from-locale-id))
(defun sort-id-from-locale-id (locale-id)
  (translate (logand #xf (ash locale-id -16)) 'lang-sort))

(declaim (inline sort-version-from-locale-id))
(defun sort-version-from-locale-id (locale-id)
  (logand #xF (ash locale-id -20)))


(defconstant lang-system-default (make-lang-id :neutral :sys-default))
(defconstant lang-user-default (make-lang-id :neutral :default))

(defconstant locale-system-default (make-locale-id
                                     lang-system-default
                                     :default))

(defconstant locale-user-default (make-locale-id
                                   lang-user-default
                                   :default))

(defconstant locale-neutral (make-locale-id
                              (make-lang-id :neutral :neutral)
                              :default))

(defconstant locale-invariant (make-locale-id
                                  (make-lang-id :invariant :neutral)
                                :default))

);;eval-when
