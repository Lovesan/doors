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

(defpackage #:doors.ui
  (:use #:cl #:alexandria #:virgil #:doors #:doors.gdi)
  (:export
    ;;window classes
    #:class-style
    #:cs-byte-align-center
    #:cs-byte-align-window
    #:cs-class-dc
    #:cs-dbl-clks
    #:cs-drop-shadow
    #:cs-global-class
    #:cs-hredraw
    #:cs-no-close
    #:cs-own-dc
    #:cs-parent-dc
    #:cs-save-bits
    #:cs-vredraw
    #:wndclass
    #:make-wndclass
    #:wndclass-style
    #:wndclass-wndproc
    #:wndclass-cls-extra
    #:wndclass-wnd-extra
    #:wndclass-instance
    #:wndclass-icon
    #:wndclass-cursor
    #:wndclass-background
    #:wndclass-menu-name
    #:wndclass-class-name
    #:wndclass*
    #:make-wndclass*
    #:wndclass-style*
    #:wndclass-wndproc*
    #:wndclass-cls-extra*
    #:wndclass-wnd-extra*
    #:wndclass-instance*
    #:wndclass-icon*
    #:wndclass-cursor*
    #:wndclass-background*
    #:wndclass-menu-name
    #:wndclass-class-name*
    #:wndclass-small-icon
    #:system-color
    #:color-3d-dk-shadow
    #:color-3d-face
    #:color-3d-highlight
    #:color-3d-hilight
    #:color-3d-light
    #:color-3d-shadow
    #:color-active-border
    #:color-active-caption
    #:color-app-workspace
    #:color-background
    #:color-btn-face
    #:color-btn-highlight
    #:color-btn-hilight
    #:color-btn-shadow
    #:color-btn-text
    #:color-caption-text
    #:color-desktop
    #:color-gradient-active-caption
    #:color-gradient-inactive-caption
    #:color-gray-text
    #:color-highlight
    #:color-highlight-text
    #:color-hotlight
    #:color-inactive-border
    #:color-inactive-caption
    #:color-inactive-caption-text
    #:color-info-bk
    #:color-info-text
    #:color-menu
    #:color-menu-hilight
    #:color-menu-bar
    #:color-menu-text
    #:color-scrollbar
    #:color-window
    #:color-window-frame
    #:color-window-text
    #:class-info
    #:class-info*
    #:class-long
    #:class-long-ptr
    #:window-class-name
    #:class-word
    #:window-long
    #:window-long-ptr
    #:register-class
    #:register-class*
    #:unregister-class
    
    ;;windows
    #:msg
    #:make-msg
    #:msg-hwnd
    #:msg-message
    #:msg-wparam
    #:msg-lparam
    #:msg-time
    #:msg-pt
    #:dispatch-message
    #:translate-message
    #:get-message
    #:post-quit-message
    
    
    ))

#|
(defpackage #:doors.nls
  (:use #:cl #:alexandria #:virgil #:doors)
  (:export
    
    ;;internationalization features
    #:lang
    #:LANG-NEUTRAL
    #:LANG-INVARIANT
    #:LANG-AFRIKAANS
    #:LANG-ALBANIAN
    #:LANG-ARABIC
    #:LANG-ARMENIAN
    #:LANG-ASSAMESE
    #:LANG-AZERI
    #:LANG-BASQUE
    #:LANG-BELARUSIAN
    #:LANG-BENGALI
    #:LANG-BULGARIAN
    #:LANG-CATALAN
    #:LANG-CHINESE
    #:LANG-CROATIAN
    #:LANG-CZECH
    #:LANG-DANISH
    #:LANG-DIVEHI
    #:LANG-DUTCH
    #:LANG-ENGLISH
    #:LANG-ESTONIAN
    #:LANG-FAEROESE
    #:LANG-FARSI
    #:LANG-FINNISH
    #:LANG-FRENCH
    #:LANG-GALICIAN
    #:LANG-GEORGIAN
    #:LANG-GERMAN
    #:LANG-GREEK
    #:LANG-GUJARATI
    #:LANG-HEBREW
    #:LANG-HINDI
    #:LANG-HUNGARIAN
    #:LANG-ICELANDIC
    #:LANG-INDONESIAN
    #:LANG-ITALIAN
    #:LANG-JAPANESE
    #:LANG-KANNADA
    #:LANG-KASHMIRI
    #:LANG-KAZAK
    #:LANG-KONKANI
    #:LANG-KOREAN
    #:LANG-KYRGYZ
    #:LANG-LATVIAN
    #:LANG-LITHUANIAN
    #:LANG-MACEDONIAN
    #:LANG-MALAY
    #:LANG-MALAYALAM
    #:LANG-MANIPURI
    #:LANG-MARATHI
    #:LANG-MONGOLIAN
    #:LANG-NEPALI
    #:LANG-NORWEGIAN
    #:LANG-ORIYA
    #:LANG-POLISH
    #:LANG-PORTUGUESE
    #:LANG-PUNJABI
    #:LANG-ROMANIAN
    #:LANG-RUSSIAN
    #:LANG-SANSKRIT
    #:LANG-SERBIAN
    #:LANG-SINDHI
    #:LANG-SLOVAK
    #:LANG-SLOVENIAN
    #:LANG-SPANISH
    #:LANG-SWAHILI
    #:LANG-SWEDISH
    #:LANG-SYRIAC
    #:LANG-TAMIL
    #:LANG-TATAR
    #:LANG-TELUGU
    #:LANG-THAI
    #:LANG-TURKISH
    #:LANG-UKRAINIAN
    #:LANG-URDU
    #:LANG-UZBEK
    #:LANG-VIETNAMESE    
    
    #:sublang
    #:SUBLANG-NEUTRAL                      ;; language neutral
    #:SUBLANG-DEFAULT                      ;; user default
    #:SUBLANG-SYS-DEFAULT                  ;; system default
    #:SUBLANG-ARABIC-SAUDI-ARABIA          ;; Arabic (Saudi Arabia)
    #:SUBLANG-ARABIC-IRAQ                  ;; Arabic (Iraq)
    #:SUBLANG-ARABIC-EGYPT                 ;; Arabic (Egypt)
    #:SUBLANG-ARABIC-LIBYA                 ;; Arabic (Libya)
    #:SUBLANG-ARABIC-ALGERIA               ;; Arabic (Algeria)
    #:SUBLANG-ARABIC-MOROCCO               ;; Arabic (Morocco)
    #:SUBLANG-ARABIC-TUNISIA               ;; Arabic (Tunisia)
    #:SUBLANG-ARABIC-OMAN                  ;; Arabic (Oman)
    #:SUBLANG-ARABIC-YEMEN                 ;; Arabic (Yemen)
    #:SUBLANG-ARABIC-SYRIA                 ;; Arabic (Syria)
    #:SUBLANG-ARABIC-JORDAN                ;; Arabic (Jordan)
    #:SUBLANG-ARABIC-LEBANON               ;; Arabic (Lebanon)
    #:SUBLANG-ARABIC-KUWAIT                ;; Arabic (Kuwait)
    #:SUBLANG-ARABIC-UAE                   ;; Arabic (U.A.E)
    #:SUBLANG-ARABIC-BAHRAIN               ;; Arabic (Bahrain)
    #:SUBLANG-ARABIC-QATAR                 ;; Arabic (Qatar)
    #:SUBLANG-AZERI-LATIN                  ;; Azeri (Latin)
    #:SUBLANG-AZERI-CYRILLIC               ;; Azeri (Cyrillic)
    #:SUBLANG-CHINESE-TRADITIONAL          ;; Chinese (Taiwan)
    #:SUBLANG-CHINESE-SIMPLIFIED           ;; Chinese (PR China)
    #:SUBLANG-CHINESE-HONGKONG             ;; Chinese (Hong Kong S.A.R., P.R.C.)
    #:SUBLANG-CHINESE-SINGAPORE            ;; Chinese (Singapore)
    #:SUBLANG-CHINESE-MACAU                ;; Chinese (Macau S.A.R.)
    #:SUBLANG-DUTCH                        ;; Dutch
    #:SUBLANG-DUTCH-BELGIAN                ;; Dutch (Belgian)
    #:SUBLANG-ENGLISH-US                   ;; English (USA)
    #:SUBLANG-ENGLISH-UK                   ;; English (UK)
    #:SUBLANG-ENGLISH-AUS                  ;; English (Australian)
    #:SUBLANG-ENGLISH-CAN                  ;; English (Canadian)
    #:SUBLANG-ENGLISH-NZ                   ;; English (New Zealand)
    #:SUBLANG-ENGLISH-EIRE                 ;; English (Irish)
    #:SUBLANG-ENGLISH-SOUTH-AFRICA         ;; English (South Africa)
    #:SUBLANG-ENGLISH-JAMAICA              ;; English (Jamaica)
    #:SUBLANG-ENGLISH-CARIBBEAN            ;; English (Caribbean)
    #:SUBLANG-ENGLISH-BELIZE               ;; English (Belize)
    #:SUBLANG-ENGLISH-TRINIDAD             ;; English (Trinidad)
    #:SUBLANG-ENGLISH-ZIMBABWE             ;; English (Zimbabwe)
    #:SUBLANG-ENGLISH-PHILIPPINES          ;; English (Philippines)
    #:SUBLANG-FRENCH                       ;; French
    #:SUBLANG-FRENCH-BELGIAN               ;; French (Belgian)
    #:SUBLANG-FRENCH-CANADIAN              ;; French (Canadian)
    #:SUBLANG-FRENCH-SWISS                 ;; French (Swiss)
    #:SUBLANG-FRENCH-LUXEMBOURG            ;; French (Luxembourg)
    #:SUBLANG-FRENCH-MONACO                ;; French (Monaco)
    #:SUBLANG-GERMAN                       ;; German
    #:SUBLANG-GERMAN-SWISS                 ;; German (Swiss)
    #:SUBLANG-GERMAN-AUSTRIAN              ;; German (Austrian)
    #:SUBLANG-GERMAN-LUXEMBOURG            ;; German (Luxembourg)
    #:SUBLANG-GERMAN-LIECHTENSTEIN         ;; German (Liechtenstein)
    #:SUBLANG-ITALIAN                      ;; Italian
    #:SUBLANG-ITALIAN-SWISS                ;; Italian (Swiss)
    #:SUBLANG-KASHMIRI-SASIA               ;; Kashmiri (South Asia)
    #:SUBLANG-KASHMIRI-INDIA               ;; For app compatibility only
    #:SUBLANG-KOREAN                       ;; Korean (Extended Wansung)
    #:SUBLANG-LITHUANIAN                   ;; Lithuanian
    #:SUBLANG-MALAY-MALAYSIA               ;; Malay (Malaysia)
    #:SUBLANG-MALAY-BRUNEI-DARUSSALAM      ;; Malay (Brunei Darussalam)
    #:SUBLANG-NEPALI-INDIA                 ;; Nepali (India)
    #:SUBLANG-NORWEGIAN-BOKMAL             ;; Norwegian (Bokmal)
    #:SUBLANG-NORWEGIAN-NYNORSK            ;; Norwegian (Nynorsk)
    #:SUBLANG-PORTUGUESE                   ;; Portuguese
    #:SUBLANG-PORTUGUESE-BRAZILIAN         ;; Portuguese (Brazilian)
    #:SUBLANG-SERBIAN-LATIN                ;; Serbian (Latin)
    #:SUBLANG-SERBIAN-CYRILLIC             ;; Serbian (Cyrillic)
    #:SUBLANG-SPANISH                      ;; Spanish (Castilian)
    #:SUBLANG-SPANISH-MEXICAN              ;; Spanish (Mexican)
    #:SUBLANG-SPANISH-MODERN               ;; Spanish (Spain)
    #:SUBLANG-SPANISH-GUATEMALA            ;; Spanish (Guatemala)
    #:SUBLANG-SPANISH-COSTA-RICA           ;; Spanish (Costa Rica)
    #:SUBLANG-SPANISH-PANAMA               ;; Spanish (Panama)
    #:SUBLANG-SPANISH-DOMINICAN-REPUBLIC   ;; Spanish (Dominican Republic)
    #:SUBLANG-SPANISH-VENEZUELA            ;; Spanish (Venezuela)
    #:SUBLANG-SPANISH-COLOMBIA             ;; Spanish (Colombia)
    #:SUBLANG-SPANISH-PERU                 ;; Spanish (Peru)
    #:SUBLANG-SPANISH-ARGENTINA            ;; Spanish (Argentina)
    #:SUBLANG-SPANISH-ECUADOR              ;; Spanish (Ecuador)
    #:SUBLANG-SPANISH-CHILE                ;; Spanish (Chile)
    #:SUBLANG-SPANISH-URUGUAY              ;; Spanish (Uruguay)
    #:SUBLANG-SPANISH-PARAGUAY             ;; Spanish (Paraguay)
    #:SUBLANG-SPANISH-BOLIVIA              ;; Spanish (Bolivia)
    #:SUBLANG-SPANISH-EL-SALVADOR          ;; Spanish (El Salvador)
    #:SUBLANG-SPANISH-HONDURAS             ;; Spanish (Honduras)
    #:SUBLANG-SPANISH-NICARAGUA            ;; Spanish (Nicaragua)
    #:SUBLANG-SPANISH-PUERTO-RICO          ;; Spanish (Puerto Rico)
    #:SUBLANG-SWEDISH                      ;; Swedish
    #:SUBLANG-SWEDISH-FINLAND              ;; Swedish (Finland)
    #:SUBLANG-URDU-PAKISTAN                ;; Urdu (Pakistan)
    #:SUBLANG-URDU-INDIA                   ;; Urdu (India)
    #:SUBLANG-UZBEK-LATIN                  ;; Uzbek (Latin)
    #:SUBLANG-UZBEK-CYRILLIC              ;; Uzbek (Cyrillic)
    
    
    #:lang-sort
    #:sort-default
    #:sort-japanese-xjis
    #:sort-japanese-unicode
    #:sort-chinese-big5
    #:sort-chinese-prcp
    #:sort-chinese-unicode
    #:sort-chinese-prc
    #:sort-chinese-bopomofo
    #:sort-korean-ksc
    #:sort-korean-unicode
    #:sort-german-phone-book
    #:sort-hungarian-default
    #:sort-hungarian-technical
    #:sort-georgian-traditional
    #:sort-georgean-modern
    
    #:make-lang-id
    #:primary-lang-id
    #:sublang-id
    #:nls-valid-locale-mask
    #:make-locale-id
    #:make-sort-locale-id
    #:lang-id-from-locale-id
    #:sort-id-from-locale-id
    #:sort-version-from-locale-id
    #:lang-system-default
    #:lang-user-default
    #:locale-system-default
    #:locale-user-default
    #:locale-neutral
    #:locale-invariant
    ))
|#
