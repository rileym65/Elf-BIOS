; *******************************************************************
; *** This software is copyright 2006 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

#ifdef TASM
#define equ .equ
#endif

; Define BIOS vectors for MCHIP ROM
#ifdef MCHIP
#define BIOS  00f00h
#define EBIOS 00800h
#endif

; Define address for standard BIOS vectors
#ifndef BIOS
#define BIOS 0ff00h
#endif

; Define address for extended BIOS vectors
#ifndef EBIOS
#define EBIOS 0f800h
#endif

#ifndef _TASM_
scall:      equ  r4                 ; register for SCALL
sret:       equ  r5                 ; register for SRET
#ifdef MCHIP
call:       equ  00fe0h             ; deprecated
ret:        equ  00ff1h             ; deprecated
#else 
call:       equ  0ffe0h             ; deprecated
ret:        equ  0fff1h             ; deprecated
#endif
#endif                                  

f_boot:     equ  (BIOS+00h)         ; boot from ide device
f_type:     equ  (BIOS+03h)         ; type 1 character to console
f_read:     equ  (BIOS+06h)         ; read 1 character from console
f_msg:      equ  (BIOS+09h)         ; type asciiz string to console
f_typex:    equ  (BIOS+0ch)         ; deprecated, just returns now
f_input:    equ  (BIOS+0fh)         ; read asciiz from console
f_strcmp:   equ  (BIOS+12h)         ; compare 2 strings
f_ltrim:    equ  (BIOS+15h)         ; trim leading spaces
f_strcpy:   equ  (BIOS+18h)         ; copy an asciiz string
f_memcpy:   equ  (BIOS+1bh)         ; copy memory
f_wrtsec:   equ  (BIOS+1eh)         ; write floppy sector (depricated)
f_rdsec:    equ  (BIOS+21h)         ; read floppy sector (depricated)
f_seek0:    equ  (BIOS+24h)         ; floppy seek to track 0 (depricated)
f_seek:     equ  (BIOS+27h)         ; floopy track seek (depricated)
f_drive:    equ  (BIOS+2ah)         ; select floppy drive (depricated)
f_setbd:    equ  (BIOS+2dh)         ; set console baud rate
f_mul16:    equ  (BIOS+30h)         ; 16-bit multiply
f_div16:    equ  (BIOS+33h)         ; 16-bit division
f_idereset: equ  (BIOS+36h)         ; reset ide device
f_idewrite: equ  (BIOS+39h)         ; write ide sector
f_ideread:  equ  (BIOS+3ch)         ; read ide sector
f_initcall: equ  (BIOS+3fh)         ; initialize R4 and R5
f_bootide:  equ  (BIOS+42h)         ; boot from ide device
f_hexin:    equ  (BIOS+45h)         ; convert ascii number to hex
f_hexout2:  equ  (BIOS+48h)         ; convert hex to 2-digit ascii
f_hexout4:  equ  (BIOS+4bh)         ; convert hex to 4-digit ascii
f_tty:      equ  (BIOS+4eh)         ; type character to console
f_mover:    equ  (BIOS+51h)         ; program relocator
f_minimon:  equ  (BIOS+54h)         ; mini monitor
f_freemem:  equ  (BIOS+57h)         ; determine memory size
f_isnum:    equ  (BIOS+5ah)         ; determine if D is numeric
f_atoi:     equ  (BIOS+5dh)         ; convert ascii to integer
f_uintout:  equ  (BIOS+60h)         ; convert unsigned integer to ascii
f_intout:   equ  (BIOS+63h)         ; convert signed integer to ascii
f_inmsg:    equ  (BIOS+66h)         ; type in-line message
f_inputl:   equ  (BIOS+69h)         ; read limited line from console
f_brktest:  equ  (BIOS+6ch)         ; check for serial break
f_findtkn:  equ  (BIOS+6fh)         ; find token in a token table
f_isalpha:  equ  (BIOS+72h)         ; determine if D is alphabetic
f_ishex:    equ  (BIOS+75h)         ; determine if D is hexadecimal
f_isalnum:  equ  (BIOS+78h)         ; determine if D is alpha or numeric
f_idnum:    equ  (BIOS+7bh)         ; determine type of ascii number
f_isterm:   equ  (BIOS+7eh)         ; determine if D is a termination char
f_getdev:   equ  (BIOS+81h)         ; get supported devices

f_version:  equ  (BIOS+0f9h)        ; 3 bytes holding bios version number

; "Extended" BIOS vectors
f_bread:    equ  (EBIOS+00h)        ; read from onboard serial port
f_btype:    equ  (EBIOS+03h)        ; write to onboard serial port
f_btest:    equ  (EBIOS+06h)        ; test onboard serial port
f_utype:    equ  (EBIOS+09h)        ; write to disk board UART
f_uread:    equ  (EBIOS+0ch)        ; read from disk board UART
f_utest:    equ  (EBIOS+0fh)        ; test disk board UART
f_usetbd:   equ  (EBIOS+12h)        ; set disk board UART baud rate and format
f_gettod:   equ  (EBIOS+15h)        ; read time of day clock
f_settod:   equ  (EBIOS+18h)        ; set time of day clock
f_rdnvr:    equ  (EBIOS+1bh)        ; read non volatile RAM
f_wrnvr:    equ  (EBIOS+1eh)        ; write non volatile RAM
f_idesize:  equ  (EBIOS+21h)        ; return size of attached IDE drive(s)
f_ideid:    equ  (EBIOS+24h)        ; return device data for IDE drive(s)
f_tmtoas:   equ  (EBIOS+2ah)        ; time to ASCII string
f_dttoas:   equ  (EBIOS+27h)        ; date to ASCII string
f_rtctest:  equ  (EBIOS+2dh)        ; test size and presence of RTC/NVR
f_astodt:   equ  (EBIOS+30h)        ; convert ASCII string to date
f_astotm:   equ  (EBIOS+33h)        ; convert ASCII string to time
