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

(in-package #:cl-user)

(defpackage #:doors
  (:use #:cl #:alexandria #:virgil)
  (:export
    ;;libraries
    #:kernel32
    #:user32
    #:gdi32
    #:ws2-32
    
    ;;windows types, constructors and accessors
    #:word
    #:dword
    #:qword
    #:wparam
    #:lparam
    #:hresult
    #:lresult
    #:long-ptr
    #:ulong-ptr
    #:handle
    #:hresult
    #:atom
    #:astring
    #:wstring
    #:tstring
    #:large-integer
    #:make-large-integer
    #:large-integer-p
    #:large-integer-low-part
    #:large-integer-high-part
    #:large-integer-quad-part
    #:large-integer-p
    #:copy-large-integer
    #:ularge-integer
    #:make-ularge-integer
    #:ularge-integer-p
    #:ularge-integer-low-part
    #:ularge-integer-high-part
    #:ularge-integer-quad-part
    #:ularge-integer-p
    #:copy-ularge-integer
    #:luid
    #:copy-luid
    #:luid-p
    #:luid-quad-part
    #:luid-low-part
    #:luid-high-part
    #:unicode-string-max-bytes
    #:unicode-string-max-chars
    #:list-entry
    #:list-entry
    #:list-entry-p
    #:copy-list-entry
    #:list-entry-flink
    #:list-entry-blink
    #:single-list-entry
    #:single-list-entry-p
    #:copy-single-list-entry
    #:single-list-entry-next
    #:rect
    #:rect-left
    #:rect-top
    #:rect-right
    #:rect-bottom
    #:point
    #:pt-x
    #:pt-y
    #:point-s
    #:pt-s-x
    #:pt-s-y
    #:size
    #:size-cx
    #:size-cy
    #:filetime
    #:filetime-low-date-time
    #:filetime-high-date-time
    
    ;;errors and conditions
    #:facility
    #:facility-null
    #:facility-rpc
    #:facility-dispatch
    #:facility-storage
    #:facility-interface
    #:facility-win32
    #:facility-windows
    #:facility-security
    #:facility-sspi
    #:facility-control
    #:facility-certification
    #:facility-internet
    #:facility-media-server
    #:facility-msmq
    #:facility-setup-api
    #:facility-smart-card
    #:facility-com+
    #:facility-aaf
    #:facility-urt
    #:facility-acs
    #:facility-direct-play
    #:facility-umi
    #:facility-sxs
    #:facility-windows-ce
    #:facility-http
    #:facility-background-copy
    #:facility-configuration
    #:facility-state-management
    #:facility-meta-directory
    #:facility-windows-update
    #:facility-directory-service

    #:make-hresult
    #:hresult-from-win32
    #:hresult-from-nt
    #:hresult-error-p
    #:hresult-facility
    #:hresult-code

    #:windows-condition
    #:windows-condition-code

    #:define-results

    #:windows-status
    #:windows-status-code

    #:status-ok
    #:status-false

    #:windows-error
    #:windows-error-code

    #:error-success
    #:error-unexpected-failure
    #:error-not-implemented
    #:error-out-of-memory
    #:error-invalid-arg
    #:error-no-interface
    #:error-invalid-pointer
    #:error-invalid-handle
    #:error-abort
    #:error-failure
    #:error-access-denied
    #:error-data-pending
    
    #:get-last-error
    #:set-last-error
    #:last-error
    #:non-system-error
    #:system-error-code-p
    
    ;;winnt version
    #:winnt-version
    #:version-suite
    #:ver-suite-backoffice
    #:ver-suite-blade
    #:ver-suite-compute-server
    #:ver-suite-datacenter
    #:ver-suite-enterprise
    #:ver-suite-embedded-nt
    #:ver-suite-personal
    #:ver-suite-single-user-ts
    #:ver-suite-small-business
    #:ver-suite-small-business-restricted
    #:ver-suite-storage-server
    #:ver-suite-terminal
    #:ver-suite-home-server
    #:ver-server-nt
    #:ver-workstation-nt
    #:version-product-type
    #:ver-nt-domain-controller
    #:ver-nt-server
    #:ver-nt-workstation
    #:os-version-info
    #:os-version-info-ex-p
    #:osverinfo-size
    #:osverinfo-major-version
    #:osverinfo-minor-version
    #:osverinfo-build-number
    #:osverinfo-platform-id
    #:osverinfo-csd-version
    #:osverinfo-service-pack-major
    #:osverinfo-service-pack-minor
    #:osverinfo-suite-mask
    #:osverinfo-product-type
    
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
    
    ;;utility functions
    #:make-short
    #:make-word
    #:make-long
    #:make-dword
    #:make-long-long
    #:make-qword
    #:low-dword
    #:high-dword
    #:low-word
    #:high-word
    #:low-byte
    #:high-byte
    
    ;;uuid and friends
    #:uuid-of
    #:uuid-null
    #:guid-null
    #:iid-null
    #:clsid-null
    #:fmtid-null
    
    #:uuid
    #:uuid-dw
    #:uuid-w1
    #:uuid-w2
    #:uuid-b1
    #:uuid-b2
    #:uuid-b3
    #:uuid-b4
    #:uuid-b5
    #:uuid-b6
    #:uuid-b7
    #:uuid-b8
    #:uuid-p
    #:copy-uuid
    #:define-uuid
    #:define-ole-uuid
    #:with-uuid-accessors
    #:uuid-equal
    
    #:guid
    #:guid-dw
    #:guid-w1
    #:guid-w2
    #:guid-b1
    #:guid-b2
    #:guid-b3
    #:guid-b4
    #:guid-b5
    #:guid-b6
    #:guid-b7
    #:guid-b8
    #:guid-p
    #:copy-guid
    #:define-guid
    #:define-ole-guid
    #:with-guid-accessors
    #:guid-equal
    
    #:iid
    #:iid-dw
    #:iid-w1
    #:iid-w2
    #:iid-b1
    #:iid-b2
    #:iid-b3
    #:iid-b4
    #:iid-b5
    #:iid-b6
    #:iid-b7
    #:iid-b8
    #:iid-p
    #:copy-iid
    #:define-iid
    #:define-ole-iid
    #:with-iid-accessors
    #:iid-equal
    
    #:clsid
    #:clsid-dw
    #:clsid-w1
    #:clsid-w2
    #:clsid-b1
    #:clsid-b2
    #:clsid-b3
    #:clsid-b4
    #:clsid-b5
    #:clsid-b6
    #:clsid-b7
    #:clsid-b8
    #:clsid-p
    #:copy-clsid
    #:define-clsid
    #:define-ole-clsid
    #:with-clsid-accessors
    #:clsid-equal
    
    #:fmtid
    #:fmtid-dw
    #:fmtid-w1
    #:fmtid-w2
    #:fmtid-b1
    #:fmtid-b2
    #:fmtid-b3
    #:fmtid-b4
    #:fmtid-b5
    #:fmtid-b6
    #:fmtid-b7
    #:fmtid-b8
    #:fmtid-p
    #:copy-fmtid
    #:define-fmtid
    #:define-ole-fmtid
    #:with-fmtid-accessors
    #:fmtid-equal
        
    #:objectid
    #:copy-objectid
    #:objectid-p
    #:objectid-lineage
    #:objectid-uniquifier
    ))
