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

(defpackage #:doors
  (:use #:cl #:alexandria #:virgil)
  (:export
    ;;libraries
    #:ntdll
    #:kernel32
    #:user32
    #:comctl32
    #:gdi32
    #:ws2-32
    #:advapi32
    #:psapi
    #:ole32
    #:oleaut32
    #:secur32
      
    ;;windows types
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
    #:valid-handle-p
    #:invalid-handle-value
    #:hresult
    #:atom
    #:invalid-atom
    #:valid-atom-p
    #:astring
    #:wstring
    #:tstring
    #:unicode-string-max-bytes
    #:unicode-string-max-chars
    #:pascal-string
    
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
    #:facility-usermode-common-logging
    #:facility-usermode-filter-manager
    #:facility-background-copy
    #:facility-configuration
    #:facility-state-management
    #:facility-meta-directory
    #:facility-windows-update
    #:facility-directory-service
    #:facility-graphics
    #:facility-shell
    #:facility-tpm-services
    #:facility-tmp-software
    #:facility-pla
    #:facility-fve
    #:facility-fwp
    #:facility-winrm
    #:facility-ndis
    #:facility-usermode-virtualization
    #:facility-usermode-volmgr
    #:facility-bcd
    #:facility-usermode-vhd
    #:facility-sdiag
    #:facility-web-services
    #:facility-windows-defender
    #:facility-opc
    #:facility-dsound
    #:facility-d3d
    #:facility-d3d10
    #:facility-dxgi
    #:facility-d3d11
    #:facility-dwrite
    #:facility-d2d
    
    #:make-hresult
    #:hresult-from-win32
    #:hresult-from-nt
    #:hresult-error-p
    #:hresult-facility
    #:hresult-code

    #:windows-condition
    #:windows-condition-code    

    #:define-results
    #:print-windows-condition

    #:windows-status
    #:windows-status-code

    #:status-ok
    #:status-false

    #:windows-error
    #:windows-error-code
    
    #:error-success
    #:error-unexpected-failure
    #:error-invalid-info-class
    #:error-out-of-memory
    #:error-not-enough-memory
    #:error-invalid-arg
    #:error-invalid-handle
    #:error-bad-length
    #:error-incufficient-buffer
    #:error-buffer-overflow
    #:error-access-denied
    #:error-more-data
    
    #:last-error
    #:invoke-last-error
    #:non-system-error
    #:system-error-code-p
    #:beep
    #:fatal-app-exit
    #:flash-window
    #:flash-window*
    #:flash-window-flags
    #:flashw-stop
    #:flashw-caption
    #:flashw-tray
    #:flashw-all
    #:flashw-timer
    #:flashw-timer-no-fg
    #:flash-window-info
    #:flash-window-hwnd
    #:flash-window-count
    #:flash-window-timeout
    #:format-message
    #:format-message-flags
    #:format-message-allocate-buffer
    #:format-message-argument-array
    #:format-message-from-module
    #:format-message-from-string
    #:format-message-from-system
    #:format-message-ignore-inserts
    #:format-message-max-width-mask
    #:system-error-mode
    #:sem-fail-critical-errors
    #:sem-no-alignment-fault-exception
    #:sem-no-page-fault-error-box
    #:sem-no-open-file-error-box
    #:error-mode
    #:thread-error-mode
    #:message-beep
    
    ;;winnt version
    #:os-version
    #:os-version*
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
    #:make-os-version-info
    #:osverinfo-major-version
    #:osverinfo-minor-version
    #:osverinfo-build-number
    #:osverinfo-platform-id
    #:osverinfo-csd-version
    #:osverinfo-service-pack-major
    #:osverinfo-service-pack-minor
    #:osverinfo-suite-mask
    #:osverinfo-product-type
    
    ;;guid and friends    
    #:guid
    #:guidp
    #:make-guid
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
    #:uuid
    #:uuid-of
    #:uuid-null
    #:string-from-guid
    #:guid-from-string
    #:invalid-guid-format
    #:invalid-guid-format-string    
    
    ;;handle stuff
    #:close-handle
    #:duplicate-handle
    #:handle-flags
    #:handle-flag-inherit
    #:handle-flag-protect-from-close
    #:handle-information
        
    ;;time stuff
    #:file-time
    #:make-file-time
    #:make-file-time*
    #:low-date-time
    #:high-date-time
    #:quad-date-time
    #:system-time
    #:make-system-time
    #:system-time-year
    #:system-time-month
    #:system-time-day-of-week
    #:system-time-day
    #:system-time-hour
    #:system-time-minute
    #:system-time-second
    #:system-time-millisecond
    #:time-zone-information
    #:make-time-zone-information
    #:time-zone-bias
    #:time-zone-standard-name
    #:time-zone-standard-date
    #:time-zone-standard-bias
    #:time-zone-daylight-name
    #:time-zone-daylight-date
    #:time-zone-daylight-bias
    #:dynamic-time-zone-information
    #:time-zone-key-name
    #:time-zone-dynamic-daylight-time-disabled-p
    #:system-time
    #:system-time-adjustment
    #:compare-file-time
    #:dos-date-time-to-file-time
    #:file-time-to-dos-date-time
    #:file-time-to-local-file-time
    #:file-time-to-system-time
    #:time-zone-id
    #:time-zone-id-unknown
    #:time-zone-id-standard
    #:time-zone-id-daylight
    #:time-zone-id-invalid
    #:local-time
    #:system-time-as-file-time
    #:system-times
    #:tick-count
    #:tick-count*
    #:time-zone-information-for-year
    #:local-file-time-to-file-time
    #:unbiased-interrupt-time
    #:system-time-to-file-time
    #:system-time-to-tz-specific-local-time
    #:tz-specific-local-time-to-system-time
    #:performance-counter
    #:performance-frequency
    
    ;;system information stuff
    #:dock-info
    #:dock-info-undocked
    #:dock-info-docked
    #:dock-info-user-supplied
    #:dock-info-user-undocked
    #:dock-info-user-docked
    #:hw-profile-info
    #:make-hw-profile-info
    #:hw-dock-info
    #:hw-profile-guid
    #:hw-profile-name
    #:processor-architecture
    #:processor-architecture-intel
    #:processor-architecture-amd64
    #:processor-architecture-ia64
    #:processor-architecture-unknown
    #:processor-type
    #:processor-intel-386
    #:processor-intel-486
    #:processor-intel-pentium
    #:processor-intel-ia64
    #:processor-amd-x86-64
    #:system-info
    #:make-system-info
    #:sysinfo-processor-architecture
    #:sysinfo-page-size
    #:sysinfo-minimum-application-address
    #:sysinfo-maximum-application-address
    #:sysinfo-active-processor-mask
    #:sysinfo-number-of-processors
    #:sysinfo-processor-type
    #:sysinfo-allocation-granularity
    #:sysinfo-processor-level
    #:sysinfo-processor-revision
    #:computer-name-format
    #:computer-name-netbios
    #:computer-name-dns-hostname
    #:computer-name-dns-domain
    #:computer-name-dns-fully-qualified
    #:computer-name-physical-netbios
    #:computer-name-physical-dns-hostname
    #:computer-name-physical-dns-domain
    #:computer-name-physical-dns-fully-qualified
    #:extended-name-format
    #:name-unknown
    #:name-fully-qualified-dn
    #:name-sam-compatible
    #:name-display
    #:name-unique-id
    #:name-canonical
    #:name-user-principal
    #:name-canonical*
    #:name-service-principal
    #:name-dns-domain
    #:dns-hostname-to-computer-name
    #:enum-system-firmware-tables
    #:expand-environment-strings
    #:computer-name
    #:computer-name*
    #:computer-object-name
    #:current-hw-profile
    #:firmware-environment-variable
    #:native-system-info
    #:product-type
    #:product-business
    #:product-business-n
    #:product-cluster-server
    #:product-datacenter-server
    #:product-datacenter-server-core
    #:product-datacenter-server-core-v
    #:product-datacenter-server-v
    #:product-enterprise
    #:product-enterprise-n
    #:product-enterprise-server
    #:product-enterprise-server-core
    #:product-enterprise-server-core-v
    #:product-enterprise-server-v
    #:product-enterprise-server-ia64
    #:product-home-basic
    #:product-home-premium
    #:product-home-premium-n
    #:product-hyper-v
    #:product-medium-business-server-management
    #:product-medium-business-server-messaging
    #:product-medium-business-server-security
    #:product-professional
    #:product-professional-n
    #:product-server-for-small-business
    #:product-server-for-small-business-v
    #:server-foundation
    #:product-small-business-server
    #:product-solution-embedded-server
    #:product-standard-server
    #:product-standard-server-core
    #:product-standard-server-core-v
    #:product-standard-server-v
    #:product-starter
    #:product-starter-n
    #:product-storage-enterprise-server
    #:product-storage-express-server
    #:product-storage-standard-server
    #:product-storage-workgroup-server
    #:product-undefined
    #:product-ultimate
    #:product-ultimate-n
    #:product-web-server
    #:product-web-server-core
    #:product-info
    #:system-directory
    #:system-firmware-table
    #:system-registry-quota
    #:system-windows-directory
    #:windows-directory
    #:system-wow64-directory
    #:user-name
    #:user-name*
    #:processor-features
    #:pf-3dnow-instructions-available
    #:pf-channels-enabled
    #:pf-compare-exchange-double
    #:pf-compare-exchange-128
    #:pf-compare-64-exchange-128
    #:pf-floating-point-emulated
    #:pf-floating-point-precision-errata
    #:pf-mmx-instructions-available
    #:pf-nx-enabled
    #:pf-pae-enabled
    #:pf-rdtsc-instruction-available
    #:pf-sse3-insructions-available
    #:pf-xmmi-instructions-available
    #:pf-xmmi64-instructions-available
    #:pf-xsave-enabled
    #:is-processor-feature-present
    #:translate-account-name
    #:verify-version-info
    #:ver-condition-mask
    
    ;;console stuff
    #:console-event
    #:event-console-caret
    #:event-console-end-application
    #:event-console-layout
    #:event-console-start-application
    #:event-console-update-region
    #:event-console-update-scroll
    #:event-console-update-simple
    #:char-attributes
    #:char-foreground-blue
    #:char-foreground-green
    #:char-foreground-red
    #:char-foreground-intensity
    #:char-background-blue
    #:char-background-green
    #:char-background-red
    #:char-background-intensity
    #:char-common-lvb-leading-byte
    #:char-common-lvb-trailing-byte
    #:char-common-lvb-grid-horizontal
    #:char-common-lvb-grid-lvertical
    #:char-common-lvb-grid-rvertical
    #:char-common-lvb-reverse-video
    #:char-common-lvb-underscore
    #:char-info
    #:make-char-info
    #:char-info-char
    #:char-info-attributes
    #:console-cursor-info
    #:make-console-cursor-info
    #:console-cursor-size
    #:console-cursor-visible
    #:coord
    #:make-coord
    #:copy-coord
    #:coord-p
    #:coord-x
    #:coord-y
    #:console-font-info
    #:make-console-font-info
    #:console-fi-font
    #:console-fi-font-size
    #:console-font-info*
    #:make-console-font-info*
    #:console-fi-font*
    #:console-fi-font-size*
    #:console-fi-font-family
    #:console-fi-font-weight
    #:console-fi-face-name
    #:console-history
    #:make-console-history-info
    #:console-history-buffer-size
    #:console-history-number-of-history-buffers
    #:console-history-flags
    #:control-key-state
    #:control-key-capslock-on
    #:control-key-enhanced-key
    #:control-key-left-alt-pressed
    #:control-key-left-ctrl-pressed
    #:control-key-numlock-on
    #:control-key-right-alt-pressed
    #:control-key-right-ctrl-pressed
    #:control-key-scrollock-on
    #:control-key-shift-pressed
    #:console-read-control
    #:make-console-read-control
    #:console-rc-initial-chars
    #:console-rc-wakeup-mask
    #:console-rc-control-key-state
    #:small-rect
    #:small-rect-left
    #:small-rect-top
    #:small-rect-right
    #:small-rect-bottom
    #:console-screen-buffer-info
    #:make-console-screen-buffer-info
    #:console-sb-size
    #:console-sb-cursor-position
    #:console-sb-attributes
    #:console-sb-window
    #:console-sb-maximum-window-size
    #:console-screen-buffer-info*
    #:make-console-screen-buffer-info*
    #:console-sb-size*
    #:console-sb-cursor-position*
    #:console-sb-attributes*
    #:console-sb-window*
    #:console-sb-maximum-window-size*
    #:console-sb-popup-attributes
    #:console-sb-fill-screen-supported
    #:console-sb-color-table
    #:console-selection-flags
    #:console-mouse-down
    #:console-mouse-selection
    #:console-no-selection
    #:console-selection-in-progress
    #:console-selection-not-empty
    #:console-selection-info
    #:make-console-selection-info
    #:console-selection-flags
    #:console-selection-anchor
    #:console-selection
    #:focus-event-record
    #:make-focus-event-record
    #:focus-event-set-focus
    #:key-event-record
    #:make-key-event-record
    #:key-event-key-down
    #:key-event-repeat-count
    #:key-event-virtual-key-code
    #:key-event-char
    #:key-event-control-key-state
    #:menu-event-record
    #:make-menu-event-record
    #:mouse-event-record
    #:make-mouse-event-record
    #:mouse-event-mouse-position
    #:mouse-event-button-state
    #:mouse-event-control-key-state
    #:mouse-event-flags
    #:window-buffer-size-record
    #:make-window-buffer-size-record
    #:window-buffer-event-size
    #:input-record
    #:make-input-record
    #:input-record-event-type
    #:input-record-event
    #:std-handle
    #:std-input-handle
    #:std-output-handle
    #:std-error-handle
    #:add-console-alias
    #:alloc-console
    #:attach-console
    #:create-console-screen-buffer
    #:fill-console-output-attribute
    #:fill-console-output-character
    #:flush-console-input-buffer
    #:free-console
    #:generate-console-ctrl-event
    #:console-alias
    #:console-aliases-length
    #:console-aliases
    #:console-alias-exes-length
    #:console-alias-exes
    #:console-input-code-page
    #:console-output-code-page
    #:console-cursor-info
    #:console-display-mode
    #:console-fullscreen
    #:console-fullscreen-hardware
    #:console-font-size
    #:console-mode
    #:enable-echo-input
    #:enable-insert-mode
    #:enable-line-io
    #:enable-mouse-input
    #:enable-processed-io
    #:enable-quick-edit-mode
    #:enable-window-input
    #:console-original-title
    #:console-process-list
    #:console-selection-info
    #:console-title
    #:console-window
    #:current-console-font
    #:current-console-font*
    #:largest-console-window-size
    #:number-of-console-input-events
    #:number-of-console-mouse-buttons
    #:peek-console-input
    #:read-console
    #:read-console-input
    #:read-console-output
    #:read-console-output-attribute
    #:read-console-output-character
    #:scroll-console-screen-buffer
    #:console-active-screen-buffer
    #:console-ctrl-handler
    #:console-cursor-position
    #:console-text-attribute
    #:console-screen-buffer-size
    #:console-window-info
    #:write-console
    #:write-console-input
    #:write-console-output
    #:write-console-output-attribute
    #:write-console-output-character
    
    ;;DLL stuff
    #:disable-thread-library-calls
    #:free-library
    #:free-library-and-exit-thread
    #:dll-directory
    #:module-file-name
    #:module-handle
    #:module-handle-flags
    #:module-handle-flag-inc-refcount
    #:module-handle-flag-pin
    #:module-handle-flag-from-address
    #:module-handle-flag-unchanged-refcount
    #:module-handle*
    #:proc-address
    #:load-library
    #:load-library-flags
    #:load-library-dont-resolve-dll-references
    #:load-library-ignore-code-authz-level
    #:load-library-as-datafile
    #:load-library-as-datafile-exclusive
    #:load-library-as-image-resource
    #:load-library-with-altered-search-path
    #:load-library*
    #:load-params
    #:make-load-params
    #:load-params-env-address
    #:load-params-cmd-line
    #:load-params-cmd-show
    #:load-module
    
    ;;processes
    #:processor-cache-type
    #:cache-unified
    #:cache-instruction
    #:cache-data
    #:cache-trace
    #:cache-descriptor
    #:make-cache-descriptor
    #:cache-level
    #:cache-associativity
    #:cache-line-size
    #:cache-size
    #:cache-type
    #:io-counters
    #:make-io-counters
    #:read-operation-count
    #:write-operation-count
    #:other-operation-count
    #:read-transfer-count
    #:write-transfer-count
    #:other-transfer-count
    #:process-information
    #:make-process-information
    #:process-info-handle
    #:process-info-thread-handle
    #:process-info-id
    #:process-info-thread-id
    #:startup-flags
    #:start-force-on-feedback
    #:start-force-off-feedback
    #:start-prevent-pinning
    #:start-run-fullscreen
    #:start-title-is-app-id
    #:start-title-is-link-name
    #:start-use-count-chars
    #:start-use-file-attribute
    #:start-use-hotkey
    #:start-use-position
    #:start-use-show-window
    #:start-use-size
    #:start-use-std-handles
    #:startup-info
    #:make-startup-info
    #:startup-info-desktop
    #:startup-info-title
    #:startup-info-x
    #:startup-info-y
    #:startup-info-x-size
    #:startup-info-y-size
    #:startup-info-x-count-chars
    #:startup-info-y-count-chars
    #:startup-info-fill-attribute
    #:startup-info-flags
    #:startup-info-show-window
    #:startup-info-stdin
    #:startup-info-stdout
    #:startup-info-stderror
    #:startup-info*
    #:make-startup-info*
    #:startup-info-attribute-list
    #:process-creation-flags
    #:create-break-away-from-job
    #:create-default-error-mode
    #:create-new-console
    #:create-new-process-group
    #:create-no-window
    #:create-protected-process
    #:create-preserve-code-authz-level
    #:create-shared-wow-vdm
    #:create-suspended
    #:create-unicode-environment
    #:create-debug-only-this-process
    #:create-debug-process
    #:create-extended-startup-info-present
    #:create-inherit-parent-affinity
    #:create-process
    #:create-process-as-user
    #:create-process-with-logon
    #:create-process-with-token
    #:exit-process
    #:flush-process-write-buffers
    #:free-environment-strings
    #:command-line
    #:current-process
    #:current-process-id
    #:current-processor-number
    #:environment-strings
    #:environment-variable
    #:process-exit-code
    #:gui-resources
    #:priority-class
    #:above-normal-priority-class
    #:below-normal-priority-class
    #:high-priority-class
    #:idle-priority-class
    #:normal-priority-class
    #:realtime-priority-class
    #:process-affinity-mask
    #:process-group-affinity
    #:process-handle-count
    #:process-id
    #:process-id-of-thread
    #:process-io-counters
    #:process-priority-boost
    #:process-shutdown-parameters
    #:process-times
    #:process-version
    #:process-working-set-size
    #:process-working-set-size*
    #:processor-system-cycle-time
    #:need-current-directory-for-exe-path
    #:process-access-flags
    #:process-access-delete
    #:process-access-read-control
    #:process-access-synchronize
    #:process-access-write-dac
    #:process-access-write-owner
    #:process-access-all
    #:process-access-create-process
    #:process-access-create-thread
    #:process-access-dup-handle
    #:process-access-query-information
    #:process-access-query-limited-information
    #:process-access-set-information
    #:process-access-set-quota
    #:process-access-suspend-resume
    #:process-access-terminate
    #:process-access-vm-operation
    #:process-access-vm-read
    #:process-access-vm-write
    #:open-process
    #:full-process-image-name
    #:process-affinity-update-mode
    #:process-cycle-time
    #:terminate-process
    
    ;;Threads
    #:current-thread-id
    
    ;;PSAPI stuff
    #:page-file-information
    #:make-page-file-info
    #:page-file-info-total-size
    #:page-file-info-total-in-use
    #:page-file-info-peak-usage
    #:module-info
    #:make-module-info
    #:module-base-of-dll
    #:module-size-of-image
    #:module-entry-point
    #:performance-information
    #:make-performance-info
    #:perf-info-commit-total
    #:perf-info-commit-limit
    #:perf-info-commit-peak
    #:perf-info-physical-total
    #:perf-info-physical-available
    #:perf-info-system-cache
    #:perf-info-kernel-total
    #:perf-info-kernel-paged
    #:perf-info-kernel-nonpaged
    #:perf-info-page-size
    #:perf-info-handle-count
    #:perf-info-process-count
    #:perf-info-thread-count
    #:process-memory-counters
    #:make-process-memory-counters
    #:process-mc-page-fault-count
    #:process-mc-working-set-size
    #:process-mc-quota-peak-paged-pool-usage
    #:process-mc-quota-paged-pool-usage
    #:process-mc-quota-peak-nonpaged-pool-usage
    #:process-mc-quota-nonpaged-pool-usage
    #:process-mc-pagefile-usage
    #:process-mc-peak-pagefile-usage
    #:process-memory-counters*
    #:make-process-memory-counters*
    #:process-mc-private-usage
    #:ws-block-protection-flags
    #:ws-block-read
    #:ws-block-execute
    #:ws-block-read/write
    #:ws-block-copy-on-write
    #:ws-block-non-cachable
    #:ws-block-guard-page
    #:working-set-block-information
    #:make-working-set-block-information
    #:ws-block-info-virtual-page
    #:ws-block-info-protection
    #:ws-block-info-share-count
    #:ws-block-info-shared-p
    #:working-set-block-information*
    #:make-working-set-block-information*
    #:ws-block-info-node
    #:ws-block-info-valid-p
    #:ws-block-info-share-count*
    #:ws-block-info-protection*
    #:ws-block-info-shared-p*
    #:ws-block-info-locked-p
    #:ws-block-info-large-page-p
    #:working-set-information*
    #:make-working-set-information*
    #:ws-info-virtual-address
    #:ws-info-virtual-attributes
    #:ws-watch-information
    #:make-ws-watch-information
    #:ws-watch-info-faulting-pc
    #:ws-watch-info-faulting-va
    #:ws-watch-information*
    #:make-ws-watch-information*
    #:ws-watch-info-faulting-thread-id
    #:empty-working-set
    #:enum-device-drivers
    #:device-driver-file-name
    #:enum-page-files
    #:enum-processes
    #:enum-process-modules
    #:list-modules-flag
    #:list-modules-default
    #:list-modules-32bit
    #:list-modules-64bit
    #:list-modules-all
    #:enum-process-modules*
    #:device-driver-base-name
    #:mapped-file-name
    #:module-base-name
    #:module-file-name*
    #:module-information
    #:performance-info
    #:process-image-file-name
    #:process-memory-info
    #:ws-changes
    #:ws-changes*
    #:initialize-process-ws-watch
    #:query-working-set
    #:query-working-set*    
    ))
