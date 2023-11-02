#include    ../include/ops.inc

; *******************************************************************
; *** This software is copyright 2005 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

; [RLA] Configuration for Mike Riley's PicoElf with Spare Time Gizmos' EPROM ...
#ifdef PICOELF
#define UART                    ; STG style UART interface
#define  UART_SELECT    6       ;  ... UART register select I/O port
#define  UART_DATA      7       ;  ... UART data I/O port
#define NVR                     ; STG style NVR/RTC chip
#define  NVR_SELECT     6       ;  ... NVR/RTC address I/O port
#define  NVR_DATA       7       ;  ... NVR/RTC data I/O port
#define EIDE                    ; to support an exteded IDE functions
#define  IDE_SELECT     2       ;  ... IDE register select I/O port
#define  IDE_DATA       3       ;  ... IDE data I/O port
#define SERP            bn2     ; PicoElf serial input is on EF2
#define SERN            b2      ;  ... and IS inverted ...
#define SERSEQ          req     ;  ...
#define SERREQ          seq     ;  ...
#define BASE            0f000h
#define ANYROM
#define EXIT_ADDR       08003h
#endif

; [RLA] Spare Time Gizmos Elf 2000 configuration ...
#ifdef ELF2K
include config.inc
#define STGROM                  ; Spare Time Gizmos' EPROM image
#define SERHDX                  ; console serial is half duplex
#define UART                    ; STG style UART interface
#define  UART_SELECT    2       ;  ... UART register select I/O port
#define  UART_DATA      3       ;  ... UART data I/O port
#define NVR                     ; STG style NVR/RTC chip
#define  NVR_SELECT     2       ;  ... NVR/RTC address I/O port
#define  NVR_DATA       3       ;  ... NVR/RTC data I/O port
#define EIDE                    ; to support an exteded IDE functions
#define  IDE_SELECT     2       ;  ... IDE register select I/O port
#define  IDE_DATA       3       ;  ... IDE data I/O port
#define SERP            b3      ; bit banged serial input on EF3
#define SERN            bn3     ;  ... and it is NOT inverted ...
#define SERSEQ          seq     ;  ...
#define SERREQ          req     ;  ...
#define KBD_DATA        7       ; PS/2 keyboard ASCII data port
#define BKBD            b2      ; branch on keyboard data ready
#define BNKBD           bn2     ;  ... no keyboard data ready
#define BASE            0f000h
#define ANYROM
#define EXIT_ADDR       08003h
#endif

#ifdef MC
#define SERP         b3
#define SERN        bn3
#define SERSEQ      seq
#define SERREQ      req
#define  IDE_SELECT   2       ;  ... IDE register select I/O port
#define  IDE_DATA     3       ;  ... IDE data I/O port
#define BASE            0f000h
#endif

#ifdef MCHIP
#define SERP        bn3
#define SERN         b3
#define SERSEQ      req
#define SERREQ      seq
#define  IDE_SELECT   2       ;  ... IDE register select I/O port
#define  IDE_DATA     3       ;  ... IDE data I/O port
#define BASE          00000h
#define ANYROM
#define EXIT_ADDR     07003h
#endif

#ifndef SERP
#define SERP        b2
#define SERN       bn2
#define SERSEQ     seq
#define SERREQ     req
#define IDE_SELECT   2       ;  ... IDE register select I/O port
#define IDE_DATA     3       ;  ... IDE data I/O port
#define BASE            0f000h
#endif

; [RLA] Other definitions ...
data:   equ     0
scall:  equ     r4
sret:   equ     r5

#ifdef VIDEO
        org     BASE+0200h      ; [RLA] the video card support needs more room
#else
        org     BASE+0300h      ; [RLA] extended BIOS starts here
#endif

; A couple of words on the baud rate constant (RE.1) usage -
;
;   If (RE.1 & 0xFE) is not zero and is not 0xFE, then the bit banged "UART" is
; in use and (RE.1 & 0xFE) determines the baud rate (its the delay constant).
; In this case the LSB of RE.1 (i.e. RE.1 & 1) is the local echo flag - if this
; is 1 then all input is echoed back to the terminal.  If it is zero, then no
; echo is performed.
;
;   On the Elf2K and the PicoElf (and maybe other future hardware platforms) if
; (RE.1 & 0xFE) is zero (i.e. the bit banged serial delay is zero!) then the
; hardware UART is used for console I/O.  The UART still obeys the local echo
; flag, so if the LSB of RE.1 is one then character input will be echoed.
;
;   On the Elf2K, and currently only on the Elf2K, if (RE.1 & 0xFE) == 0xFE
; (i.e. the slowest possible bit banged baud ate) then the Elf 2000 80 column
; video card and PS/2 keyboard interface are used for console I/O.  The local
; echo flag still works in this case as well, so PS/2 keyboard input will be
; echoed to the video card only if the LSB of RE.1 is set.

#ifdef UART
; ****************************************
; *** Test to see if uart is installed ***
; *** DF=1 uart is installed           ***
; *** DF=0 uart is not installed       ***
; ****************************************
uart_test: sex     r3               ; [RLA] output immediate data
           out     UART_SELECT      ; [RLA] select the UARTs MCR
           db      14h              ; [RLA] ...
           out     UART_DATA        ; [RLA] enable loopback mode, turn all
           db      10h              ; [RLA]  ... modem control bits OFF
           out     UART_SELECT      ; [RLA] now select the modem status register
           db      16h              ; [RLA] ...
           sex     r2               ; [RLA] point to the stack again
           inp     UART_DATA        ; [RLA] read the MSR
           ani     0f0h             ; [RLA] check the current modem status
           bnz    no_uart           ; [RLA] all bits should be zero
           sex     r3               ; [RLA] back to X=P
           out     UART_SELECT      ; [RLA] select the MCR again
           db      14h              ; [RLA] ...
           out     UART_DATA        ; [RLA] enable loopback mode, turn all
           db      1fh              ; [RLA]  ... modem control bits ON
           out     UART_SELECT      ; [RLA] select the MSR
           db      16h              ; [RLA] ...
           sex     r2               ; [RLA] ...
           inp     UART_DATA        ; [RLA] read the MSR
           ani     0f0h             ; [RLA] check the current modem status
           xri     0f0h             ; [RLA] all bits should be one this time
           bnz    no_uart           ; [RLA] no uart if they arent
           sex     r3               ; [RLA] X=P
           out     UART_SELECT      ; [RLA] select the MCR once more
           db      14h              ; [RLA] ...
           out     UART_DATA        ; [RLA] disable loopback mode, set DTR & RTS
           db      03h              ; [RLA] ...
yes_uart:  smi     0                ; signal uart is present
           sep     sret             ; and return
no_uart:   adi     0                ; signal no uart installed
           sep     sret             ; and return


; ***************************************************
; *** Elf 2000 version of timalc, selects between ***
; *** bit-banged and uart serial modes            ***
; *** Returns RE.1 = 0 Uart was selected          ***
; ***         RF.0 = comm port parameters         ***
; ***         RE.1 <> 0 bit-banged baud constant  ***
; ***************************************************
e2k_timalc: sep    scall               ; see if uart is present
            dw     uart_test
            lbnf   timalc_o            ; use standard timalc if no uart
            ldi    34h                 ; set 9600 N81
            sep    scall               ; set deafult baud rate
            dw     e2k_stbd
e2k_sblp2:  sex    r3                  ; [RLA] address the data register
            out    UART_SELECT         ; [RLA] ...
            db     10h                 ; [RLA] ...
            sex    r2                  ; [RLA] ...
            inp    UART_DATA           ; [RLA] read RBR to clear DR bit
            sex    r3                  ; need to set uart register to line stat
            out    UART_SELECT
            db     015h
            sex    r2                  ; point X back to stack
e2k_sblp1:  SERN   e2k_setbd1          ; jump if character detectd on EF line
            inp    UART_DATA           ; read modem status
            shr                        ; shift DR bit into carry
            lbnf   e2k_sblp1           ; loop if no character detected on uart
            sex    r3                  ; setup for immediate out
            out    UART_SELECT         ; select uart data register
            db     010h
            sex    r2                  ; point X back to stack
            inp    UART_DATA           ; read received char to stack

;   We have already initialized the UART to 9600bps and we know that the
; operator is typing a carriage return character on the terminal connected
; to the UART (at least he is supposed to be!).  Obviously, if his terminal
; is set to 9600bps we will actually receive a 0Dh character in the UART
; buffer.  But even if his terminal is set to a faster or slower baud
; rate, assuming that he is still typing carriage return, the bit pattern
; we receive is equally unique.  For example, if he transmits CR at 4800 bps
; and we receive at 9600bps, well actually see 0E6h in the buffer.  So
; by looking up whatever value we receive in a table, we can determine the
; correct abud rate.
        ldi        high abdtab       ; [RLA] point to the auto baud table
        phi        rf                ; [RLA] ...
        ldi        low abdtab        ; [RLA] ...
        plo        rf                ; [RLA] ...
abdlp1: ldn        rf                ; [RLA] get a byte from auto baud table
        lbz        e2k_timalc        ; [RLA] start over if end of table
        inc        rf                ; [RLA] ...
        xor                          ; [RLA] compare it to byte on the stack
        lbz        abd2              ; [RLA] branch if we found a match
        inc        rf                ; [RLA] skip the second byte
        br         abdlp1            ; [RLA] and keep looking

;   Here if we find a match in the auto baud table.  Remember that were
; receiving at a fairly fast rate (9600bps) and if the operator was sending
; at a slow rate (e.g. 2400bps) then his UART is _probably still sending_
; right now!  To avoid having the last half of the CR show up as a garbage
; character, we have to delay long enough for the transmitter to finish,
; worst case.   BTW, at 4MHz, each clock is 500ns and each machine cycle
; is 4us.  At 1200 baud, 10 bits takes about 8ms to transmit, so a delay
; of about 8ms/4us = 2000 cycles is about enough.
abd2:   ldi        0                 ; [RLA] clear the delay counter
abd2a:  nop                          ; [RLA] 3 cycles
        nop                          ; [RLA] 3 cycles
        smi        1                 ; [RLA] 2 cycles
        bnz        abd2a             ; [RLA] 2 cycles
                                     ; [RLA] 10 cycles * 256 = 2560 cycles...

;   The UART data register is still selected - read the RBR to remove any
; garbage character that might be there...
        inp        UART_DATA         ; [RLA] read the data register first
        sex        r3                ; [RLA] ...
        out        UART_SELECT       ; [RLA]  ... address the LSR
        db         015h              ; [RLA]  ...
        sex        r2                ; [RLA]         ...
        inp        UART_DATA         ; [RLA]        and clear the error flags too

; [RLA] All done - return re.1 ==1 and rf.0 == baud rate...
        ldi        1                 ; [MHR] turn echoing on by default
        phi        re                ; [RLA] ...
        ldn        rf                ; [RLA] get the UART mode byte
        plo        rf                ; [RLA] return it in rf
        sep        scall             ; [RLA] change the UART mode
        dw         e2k_stbd          ; [RLA] ...
        lbr        timalc_rt         ; [RLA] and return

; Table of autoboad results and baud rates...
abdtab: dw        00d34h             ; [RLA]  9,600bps
        dw        0f235h             ; [RLA] 19,200bps
        dw        0f935h             ; [RLA] 19,200bps
        dw        0e633h             ; [RLA]  4,800bps
        dw        07832h             ; [RLA]  2,400bps
;       dw        08031h             ; [RLA]  1,200bps (unreliable!)
        db        0                  ; [RLA] end of table

; Here if we detect activity on the bit banged port...
e2k_setbd1: lbr    end_sb            ; use standard bit-banged serial


; *******************************************
; *** Implement RE.1 for serial selection ***
; *******************************************
e2k_brk: ghi      re                 ; get baud constant
         ani      0feh               ; [RLA] mask out echo bit
         lbz      f_utest            ; [RLA] branch if the UART is in use
#ifdef VIDEO
         xri      0feh               ; [RLA] is the ELF2K video card active?
         lbz      e2k_ptest          ; [RLA] yes, test PS/2 keyboard
#endif
         lbr      f_btest            ; [RLA] otherwise use the bit banged serial



; [RLA] Generic transmit routine ...
; [RLA]    This will send the character to the UART, video card or
; [RLA] the plain old bit banged serial port, depending on which
; [RLA] was detected ...
e2k_tx: plo     re              ; save character
        ghi     re              ; get baud constant
        ani     0feh            ; [RLA] mask out echo bit
        lbz     e2k_tx1         ; [RLA] jump if UART
#ifdef VIDEO
        xri     0feh            ; [RLA] is the Elf2K video card active?
        lbz     e2k_tx2         ; [RLA] yes - use 80 column video card
#endif
        glo     re              ; recover character
        lbr     f_btype         ; jump to bit-banged code
e2k_tx1:glo     re              ; recover character
        lbr     f_utype         ; jump to UART routine
#ifdef VIDEO
e2k_tx2:glo     re              ; [RLA] recover the character
        lbr     vtputc          ; [RLA] and print it on the CRT
#endif


; [RLA] Generic receive routine ...
; [RLA]    Receive a character from the UART, PS/2 keyboard or bit
; [RLA] banged serial depending on which is installed...
e2k_rx: ghi     re              ; get baud constant
        ani     0feh            ; [RLA] mask out echo bit
        lbz     f_uread         ; [RLA] use the UART if it is zero
#ifdef VIDEO
        xri     0feh            ; [RLA] is the Elf2K PS/2 keyboard active?
        lbz     e2k_pread       ; [RLA] if its zero, use PS/2
#endif
        lbr     f_bread         ; jump to bit banged code


; *********************************************
; *** Read byte from Elf 2000 PS/2 Keyboard ***
; *** Returns: D - byte read                ***
; *********************************************
#ifdef VIDEO
e2k_pread:
        BNKBD   $               ; [RLA] wait for data ready flag
        inp     KBD_DATA        ; [RLA] read the PS/2 data port
        plo     re              ; [RLA] save the character for a minute
        ghi     re              ; [RLA] get the baud rate constant
        shr                     ; [RLA] shift the echo bit into DF
        glo     re              ; [RLA] restore the data read
        lbdf    vtputc          ; [RLA] echo it and return
        sep     sret            ; [RLA] no echo - just return

; ****************************************************
; *** Test to see if character available from PS/2 ***
; *** Returns: DF=1 - character available          ***
; ****************************************************
e2k_ptest:
        adi     0               ; [RLA] clear DF
        BNKBD   ptest1          ; [RLA] PS/2 data available is EF2
        smi     0               ; [RLA] its set - set DF
ptest1: sep     sret            ; [RLA] and return
#endif


; ****************************************************
; *** Test to see if character available from UART ***
; *** Returns: DF=1 - character available          ***
; ****************************************************
e2k_utest: ldi     015h                ; need UART line status register
           str     r2                  ; prepare for out
           out     UART_SELECT         ; write to register select port
           dec     r2                  ; back to a free spot
           inp     UART_DATA           ; read line status register
           shr                         ; shift data ready bit into DF
           sep     sret                ; and return

; ************************************
; *** Read byte from Elf 2000 UART ***
; *** Returns: D - byte read       ***
; ************************************
e2k_uread:
        ldi     015h            ; need UART line status register
        str     r2              ; prepare for out
        out     UART_SELECT     ; write to register select port
        dec     r2              ; correct for inc on out
uread_lp:
        inp     UART_DATA       ; read line status register
        ani     1               ; mask for data ready bit
        lbz     uread_lp        ; loop back if no bytes to read
        ldi     010h            ; select data register
        str     r2              ; prepare for out
        out     UART_SELECT     ; write to register select port
        dec     r2              ; back to free spot
        inp     UART_DATA       ; read UART data register
        plo     re              ; save for a moment
        ghi     re              ; need to check for echo
        shr                     ; shift echo flag into DF
        glo     re              ; recover read byte
        lbdf    e2k_utype       ; jump if need echo
        sep     sret            ; otherwise return

;[MHR]     lbz     e2k_uread           ; [RLA] ignore nulls
;[RLA]     xri     $ff                 ; [RLA] and ignore $FF bytes
;[RLA]     lbz     e2k_uread           ; [RLA] ....
;[RLA]     xri     $ff                 ; [RLA] ...
;[RLA]     sep     sret                ; return byte to caller
;[RLA] Fall into utype to echo the character we just read...


; ***********************************
; *** Write byte to Elf 2000 UART ***
; *** D - byte to write           ***
; ***********************************
e2k_utype: stxd                        ; save byte to send onto stack
           ldi     015h                ; need line status register of UART
           str     r2                  ; write to stack
           out     UART_SELECT         ; select UART port
           dec     r2                  ; keep stack at empty spot
utype_lp:  inp     UART_DATA           ; read status port
           ani     020h                ; mask for holding register empty
           lbz     utype_lp            ; loop until allowd to send character
           ldi     010h                ; select data register
           str     r2                  ; prepare for out
           out     UART_SELECT         ; select UART register
           ldx                               ; [RLA] reload the original byte
           out     UART_DATA           ; write byte to UART
           dec     r2                  ; correct for inc on OUT
           sep     sret                ; and return to caller

; *****************************************
; *** Setup UART parameters             ***
; *** D = Comm parameters               ***
; ***  Bits 0-2 = 0 0 0 - 300 baud      ***
; ***             0 0 1 - 1200 baud     ***
; ***             0 1 0 - 2400 baud     ***
; ***             0 1 1 - 4800 baud     ***
; ***             1 0 0 - 9600 baud     ***
; ***             1 0 1 - 19200 baud    ***
; ***             1 1 0 - 38400 baud    ***
; ***             1 1 1 - 76800 baud    ***
; ***  bit 3 - reserved for future baud ***
; ***          rates                    ***
; ***  Bits 4-5 = 0 0 - 5 bit words     ***
; ***             0 1 - 6 bit words     ***
; ***             1 0 - 7 bit words     ***
; ***             1 1 - 8 bit words     ***
; ***  Bit    6 =   0 - no parity       ***
; ***               1 - parity enabled  ***
; ***  Bit    7 -   0 - odd parity      ***
; ***               1 - even parity     ***
; *** Returns: DF=1 - No UART installed ***
; ***          DF=0 - success           ***
; *****************************************
e2k_stbd:  plo     re                  ; save a coyp of parameters
           sex     r3                  ; setup line control register of UART
           out     UART_SELECT
           db      013h
           sex     r2                  ; set x back to stack
;[RLA]     xri     0ffh                ; check for existance of UART
;[RLA]     lbz     no_uart             ; jump if not installed
;[RLA]     xri     0ffh                ; reset value
           ldi     080h                ; [RLA] set DLAB bit
           str     r2                  ; store for write
           out     UART_DATA           ; write line control register
           dec     r2                  ; compensate for out
;[RLA]     dec     r2                  ; need some workspace
           glo     rf                  ; [RLA] save consumed register
           stxd                        ; [RLA]
           ghi     rf                  ; [RLA]
           stxd                        ; [RLA]
           glo     re                  ; get com parameters
           ani     7                   ; strip all but baud rate bits
           shl                         ; multiply by 2
           str     r2                  ; and prepare for add
           ldi     low baudtab         ; point to diviser table
           add                         ; add in offset
           plo     rf                  ; place into rf
           ldi     high baudtab        ; high portion of address
           adci    0                   ; [RLA] propagate carry
           phi     rf
           sex     r3                  ; setup for output
           out     UART_SELECT         ; select msb of diviser latch
           db      011h
           sex     rf                  ; point to diviser value
           out     UART_DATA           ; [RLA] and write to latch
           sex     r3                  ; setup for next register select
           out     UART_SELECT         ; select lsb of diviser latch
           db      010h
           sex     rf                  ; set x to diviser table
           out     UART_DATA           ; and write to uart
           sex     r2                  ; set x back to stack
;[RLA]     inc     r2                  ; point back to line control value
           irx                         ; [RLA] restore rf
           ldxa                        ; [RLA]
           phi     rf                  ; [RLA] 
           ldx                         ; [RLA]
           plo     rf                  ; [RLA]
           glo     re                  ; recover comm parameters
           shr                         ; shift out baud value
           shr
           shr
           shr
           ani     0fh                 ; [RLA] clear unneded bits
           str     r2                  ; prepare for out
           sex     r3                  ; need to write register select
           out     UART_SELECT         ; select line control register
           db      013h
           sex     r2                  ; X back to stack
           out     UART_DATA           ; write new line control register
           dec     r2                  ; compensate for out
           sex     r3                  ; [RLA] X=P (again!)
           out     UART_SELECT         ; [RLA] address modem control register
           db      014h                ; [RLA] ...
           out     UART_DATA           ; [RLA] clear loopback, set DTR & RTS
           db      03h                 ; [RLA] ...
           sex     r2                  ; [RLA] back to the stack again
           adi     0                   ; signal success
           sep     sret                ; return to caller
baudtab:   dw      512,128,64,32,16,8,4,2 ; [RLA]
#endif  ;; "#ifdef UART" ...

#ifdef NVR
; *********************************
;   Read a byte from an RTC/NVR register.  The address of the register
; is passed inline, after the CALL RTCRDI, and the contents of the RTC/NVR
; register is returned in D...
; *********************************
rtcrdi: sex     r6              ; [RLA] point to the inline arguments
        out     NVR_SELECT      ; [RLA] select the disk/uart/rtc register
        sex     r2              ; [RLA] back to the regular stack
        inp     NVR_DATA        ; [RLA] and read the RTC register 
        sep     sret            ; [RLA] return the register value in D

; *********************************
;   Write a byte to an RTC/NVR register.  The address of the register is
; passed inline, after the CALL RTCWRI, and the byte to be written is passed
; in the D register...
; *********************************
rtcwri: sex     r6              ; [RLA] address the inline arguments
        out     NVR_SELECT      ; [RLA] output the register select and skip it
        sex     r2              ; [RLA] back to the regular stack now
        str     r2              ; [RLA] temporarily store the value to output
        out     NVR_DATA        ; [RLA] and write it to the RTC/NVR 
        dec     r2              ; [RLA] out increments R(X), so fix that
        sep     sret            ; [RLA] and return

; *********************************
; *** Test if RTC is present    ***
; *** Returns: DF=1 present     ***
; ***          DF=0 not present ***
; ***          d = size         *** 
; [RLA]   Note that "size" does not include the bytes reserved for the
; [RLA] clock/calender!  i.e. a "128 byte chip" returns 114 and a "64 byte"
; [RLA] chip returns 50.  This way the size represents the number of bytes
; [RLA] actually usable by the caller, and also, it represents the maximum
; [RLA] length (less checksum) that can be passed to rdnvr/wrnvr...
; *********************************
rtctest: sep     scall           ; [RLA] read RTC register 127 first
         dw      rtcrdi          ; [RLA] ...
         db      80h+127         ; [RLA] ...
         stxd                    ; [RLA] save RTC loc 127 on the stack
         sep     scall           ; [RLA] now save RTC register 63
         dw      rtcrdi          ; [RLA] ... same as before
         db      80h+63          ; [RLA] ...
         stxd                    ; [RLA] push RTC loc 63 too

;   First test to see whether the RTC/NVR exists at all by trying to write
; (and successfully read back!) location 63...
        xri     0ffh            ; [RLA] complement previous data in loc 63
        sep     scall           ; [RLA] write it back
        dw      rtcwri          ; [RLA] ...
        db      80h+63          ; [RLA] ... to location 63
        sep     scall           ; [RLA] now try to read RTC loc 63
        dw      rtcrdi          ; [RLA] ...
        db      80h+63          ; [RLA] ...
        inc     r2              ; [RLA] previous contents are on the stack
        add                     ; [RLA] a number plus its complement
        dec     r2              ; [RLA] protect original contents of loc 63
        xri     0ffh            ; [RLA] ... always equals 0FFH
        lbnz    nortc           ; [RLA] branch if no RTC present

; Next, check the battery status bit in register 0dh...
        sep     scall           ; [RLA] read register 0x0d
        dw      rtcrdi          ; [RLA] ...
        db      80h+0dh         ; [RLA] ...
        ani     80h             ; [RLA] is the VRT bit set?
        lbz     nortc           ; [RLA] branch if the battery is dead

;   There are two versions of the NVR - a 64 byte version and a 128 byte
; version.  The 64 byte version simply doesn't decode the upper address bit,
; so with this chip an attempt to access locations 64..127 simply accesses
; locations 0..63 instead.  We can test for a 64 byte chip by writing location
; 127 and checking to see if location 63 changes...
        ldi     00              ; [RLA] write 0 to location 63
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+63          ; [RLA] ...
        ldi     0ffh            ; [RLA] then write 0FFH to location 127
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+127         ; [RLA] ...
        sep     scall           ; [RLA] now read back location 63
        dw      rtcrdi          ; [RLA] ...
        db      80h+63          ; [RLA] ...
        lbnz    rtc64           ; [RLA] branch if 64 byte chip

; Here for the 128 byte RTC/NVR with a good battery!
rtc128: irx                     ; [RLA] pop the previous contents of loc 63
        ldx                     ; [RLA] ...
        sep     scall           ; [RLA] and write it back
        dw      rtcwri          ; [RLA] ...
        db      80h+63          ; [RLA] ...
        irx                     ; [RLA] now pop location 127
        ldx                     ; [RLA] ...
        sep     scall           ; [RLA] and write that back too
        dw      rtcwri          ; [RLA] ...
        db      80h+127         ; [RLA] ...
        ldi     128-14          ; [RLA] return the size of the NVR
        smi     0               ; [RLA] and DF=1 to indicate success
        sep     sret            ; [RLA] ...


; Here for the 64 byte RTC/NVR....
rtc64:  irx                     ; [RLA] pop the previous contents of loc 63
        ldxa                    ; [RLA] ...
        sep     scall           ; [RLA] and write it back
        dw      rtcwri          ; [RLA] ...
        db      80h+63          ; [RLA] ...
                                ; [RLA] no need to restore location 127
        ldi     64-14           ; [RLA] return the size of the NVR
        smi     0               ; [RLA] and DF=1 to indicate success
        sep     sret            ; [RLA] ...

; And here if the RTC doesnt exist or the battery is dead...
nortc:  irx                     ; [RLA] pop two bytes off the stack
        irx                     ; [RLA] ...
        ldi     0               ; [RLA] return the size of the NVR
        adi     0               ; [RLA] and DF=0 to indicate failure
        sep     sret            ; [RLA] ..


; ******************************** 
; *** Read RTC date            ***
; *** RF - buffer for time     ***
; *** Returns: DF=0 - success  ***
; ***          DF=1 - error    ***
; ***             D=0 no RTC   ***
; ***             D=1 bad time ***
; ********************************
e2k_gtod:   sep     scall               ; see if RTC is present
            dw      rtctest
            bdf     rtc_go              ; jump if RTC was found
; Here if there is no RTC installed...
rtc_err:    smi     0                   ; signal error
            ldi     0                   ; as no RTC
            sep     sret                ; return
; And here if the RTC is not set...
rtc_notset: ldi     1                   ; [RLA] signal bad time/date
            smi     0                   ; [RLA] signal error 
            sep     sret                ; [RLA] return

;   There are two caveats in reading the clock - the first is that we check
; to ensure that 1) the clock is running and that 2) the 24 hour and binary
; mode bits are set.  If theres been a previous call to e2k_stod then these
; will all be true.  Note that you cant really change these bits while the
; clock is running (it can cause anomalies in the count if you do) - you have
; to first set the mode bits and then set the time, so thats why we leave it
; all up to e2k_stod().
rtc_go: sep     scall           ; [RLA] first read register 0x0a
        dw      rtcrdi          ; [RLA] ...
        db      80h+0ah         ; [RLA] ...
        ani     70h             ; [RLA] check the DV2/1/0 bits
        xri     20h             ; [RLA] make sure the clock is running
        bnz     rtc_notset      ; [RLA] not runing otherwise
        sep     scall           ; [RLA] now read register 0x0b
        dw      rtcrdi          ; [RLA] ...
        db      80h+0bh         ; [RLA] ...
        ani     06h             ; [RLA] make sure the DM and 24 bits are set
        xri     06h             ; [RLA] (both bits must be set!)
        bnz     rtc_notset      ; [RLA] clock is not set otherwise

;   The second caveat is that we have to be careful _when_ we read the clock.
; Remember, the RTC hardware potentially changes the seconds, minutes, hours
; day, month and year registers any time the clock ticks (i.e. at 23:59:59 on
; the last day of the year, all these bytes will change on the next tick!).
; If the clock just happens to tick while were in the middle of reading it,
; then the date/time we assemble can be off by a minute, an hour, even a year
; if were unlucky!  (You might think this is unlikely to happen, and I admit
; that it is, but once upon a time I was personally inveolved in fixing a bug
; in an embedded system caused by just this situation!  It happens...)
; Fortunately for us, the DS1287 designers thought of this and they provide
; us with a bit, UIP, to signal that an update coming soon.  As long as UIP=0
; were guaranteed at least 244us before an update occurs.
rtc_w1: sep     scall           ; [RLA] UIP is in register 0x0a
        dw      rtcrdi          ; [RLA] ...
        db      80h+0ah         ; [RLA] ...
        ani     80h             ; [RLA] wait for UIP to be clear
        bnz     rtc_w1          ; [RLA] ...

; Ok, were safe...  Read the clock...
        sep     scall           ; [RLA] first the month
        dw      rtcrdi          ; [RLA] ...
        db      80h+08h         ; [RLA] ...
        str     rf              ; [RLA] store that in the buffer
        inc     rf              ; [RLA] and on to the next byte
        sep     scall           ; [RLA] next the day of the month
        dw      rtcrdi          ; [RLA] ...
        db      80h+07h         ; [RLA] ...
        str     rf              ; [RLA] ...
        inc     rf              ; [RLA] ...
        sep     scall           ; [RLA] year (two digits only)
        dw      rtcrdi          ; [RLA] ...
        db      80h+09h         ; [RLA] ...
        str     rf              ; [RLA] ...
        inc     rf              ; [RLA] ...
        sep     scall           ; [RLA] hour (24 hour mode)
        dw      rtcrdi          ; [RLA] ...
        db      80h+04h         ; [RLA] ...
        str     rf              ; [RLA] ...
        inc     rf              ; [RLA] ...
        sep     scall           ; [RLA] minute
        dw      rtcrdi          ; [RLA] ...
        db      80h+02h         ; [RLA] ...
        str     rf              ; [RLA] ...
        inc     rf              ; [RLA] ...
        sep     scall           ; [RLA] and seconds
        dw      rtcrdi          ; [RLA] ...
        db      80h+00h         ; [RLA] ...
        str     rf              ; [RLA] ...
        inc     rf              ; [RLA] ...

; All is well - return DF=0...
        adi     0               ; [RLA] indicate succes
        sep     sret            ; [RLA] ...



; ********************************
; *** Set RTC date             ***
; *** RF - buffer holding data ***
; *** Returns: DF=0 - success  ***
; ***          DF=1 - no RTC   ***
; ********************************
e2k_stod:  sep     scall               ; see if RTC is present
           dw      rtctest
           lbnf    rtc_err             ; jump if no RTC is present

;   The RTC ships from the factory with the clock turned off - this saves
; the shelf life of the lithium cell.  Before setting the clock, lets turn
; on the oscillator so that it will actually keep time :-)  If the oscillator
; is already on, this will do no harm...
        ldi     20h             ; [RLA] turn ON the clock
        sep     scall           ; [RLA] ... and OFF the SQW output
        dw      rtcwri          ; [RLA] ... which we dont use anyway
        db      80h+0ah         ; [RLA] ...

;   Now, set the SET bit, which inhibits the clock from counting.  This
; prevents it from accidentally rolling over while were in the middle of
; updating the registers!  At the same time, select 24 hour mode, binary
; (not BCD) mode, and enable daylight savings time.  The latter choice is
; debatable since the chip only knows the DST rules for the USA, and not
; even all parts of the US observe DST to start with.  Still, it works for
; most customers!
        ldi     86h             ; [RLA] SET, DM, and 24hr
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+0bh         ; [RLA] ...

;   Load the clock registers.  Note that there is no error checking on the
; values - if the caller gives bogus values then the count will be
; unpredictable!
        lda     rf              ; [RLA] fetch the month
        sep     scall           ; [RLA] ... and update the RTC chip
        dw      rtcwri          ; [RLA] ...
        db      80h+08h         ; [RLA] ...
        lda     rf              ; [RLA] next the day of the month
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+07h         ; [RLA] ...
        lda     rf              ; [RLA] the year
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+09h         ; [RLA] ...
        lda     rf              ; [RLA] hour
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+04h         ; [RLA] ...
        lda     rf              ; [RLA] minute
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+02h         ; [RLA] ...
        lda     rf              ; [RLA] seconds
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+00h         ; [RLA] ...

; Clear the SET bit to allow the clock to run, and were done!
        ldi     07h             ; [RLA] DM, 24hr and DSE
        sep     scall           ; [RLA] ...
        dw      rtcwri          ; [RLA] ...
        db      80h+0bh         ; [RLA] ...
        adi     0               ; [RLA] return DF=0 for success
        sep     sret            ; [RLA] ...


; ************************************************
; *** Read bytes from NVR                      ***
; *** RF.0 - relative address to start reading ***
; *** RD - Destination for bytes               ***
; *** RC.0 - count                             ***
; *** Returns: DF=0 - success                  ***
; ***          DF=1 - no NVR                   ***
; ************************************************
e2k_rdnvr: sep     scall               ; check for presence of NVR
           dw      rtctest
           lbnf    rtc_err             ; jump if not
           glo     rf                  ; save RF
           stxd
           ghi     rf
           stxd
           glo     rd                  ; save RD
           stxd
           ghi     rd
           stxd
           sep     scall               ; compute checksum
           dw      nvr_chk
           sep     scall               ; get current checksum
           dw      get_chk
           glo     rd                  ; compare them
           str     r2
           glo     rf
           sm
           lbnz    chk_err             ; jump on checksum error
           ghi     rd                  ; check high byte as well
           str     r2
           ghi     rf
           sm
           lbnz    chk_err
           irx                         ; recover RD and RF
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rf
           ldx
           plo     rf
           adi     08eh                ; offset to beginning of general regs.
           str     r2                  ; store into memory
rdnvr_lp:  glo     rc                  ; see if done
           lbz     rdnvr_dn            ; jump if so
           out     NVR_SELECT          ; write next register address
           dec     r2                  ; compensate for inc on out
           sex     rd                  ; set x to destination memory
           inp     NVR_DATA            ; read NVR
           inc     rd                  ; increment destination
           dec     rc                  ; decrement count
           sex     r2                  ; point x back to stack
           ldi     1                   ; [RLA] increment the register address
           add                         ; [RLA]
           str     r2                  ; [RLA]
           lbr     rdnvr_lp            ; loop back for more
rdnvr_dn:  adi     0                   ; signal success
           sep     sret                ; and return
chk_err:   irx                         ; recover RD and RF
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rf
           ldx
           plo     rf
           smi     0                   ; signal an error
           ldi     1                   ; signal bad checksum
           sep     sret                ; return to caller

; ************************************************
; *** Write bytes to NVR                       ***
; *** RF.0 - relative address to start writing ***
; *** RD - source for bytes                    ***
; *** RC.0 - count                             ***
; *** Returns: DF=0 - success                  ***
; ***          DF=1 - no NVR                   ***
; ************************************************
e2k_wrnvr: sep     scall               ; check for presence of NVR
           dw      rtctest
           lbnf    rtc_err             ; jump if not
           glo     rf                  ; get starting register
           adi     08eh                ; offset to beginning of general regs.
           str     r2                  ; store into memory
wrnvr_lp:  glo     rc                  ; see if done
           lbz     wrnvr_dn            ; jump if so
           out     NVR_SELECT          ; write next register address
           dec     r2                  ; compensate for inc on out
           sex     rd                  ; set x to source memory
           out     NVR_DATA            ; write NVR
           dec     rc                  ; decrement count
           sex     r2                  ; point x back to stack
           ldi     1                   ; [RLA] increment the register address
           add                         ; [RLA]
           str     r2                  ; [RLA]
           lbr     wrnvr_lp            ; loop back for more
wrnvr_dn:  sep     scall               ; compute new checksum
           dw      nvr_chk
           sep     scall               ; and write to NVR
           dw      set_chk
           adi     0                   ; signal success
           sep     sret                ; and return

; ************************************
; *** Compute checksum for NVR ram ***
; *** Returns: RF - checksum       ***
; ************************************
nvr_chk:   ldi     0                   ; set initial checksum
           phi     rf
           plo     rf
           glo     rc                  ; [RLA] preserve rc.0
           stxd                        ; [RLA] ...
           ldi     08eh                ; starting register
           stxd                        ; [RLA]
           sep     scall               ; get size of nvram
           dw      rtctest
           adi     08eh-2              ; [RLA] setup end
           plo     rc
           inc     r2                  ; [RLA] point to register address
nvr_chklp: ldx                         ; [RLA] duplicate the byte on the TOS
           dec     r2                  ; [RLA] ...
           str     r2                  ; [RLA] ...
           out     NVR_SELECT          ; select NVR address
;[RLA]     dec     r2                  ; [RLA] correct for out increment
           dec     r2                  ; [RLA] and then point to a free byte
           inp     NVR_DATA            ; [RLA] read NVR byte
           glo     rf                  ; add into checksum
           add
           plo     rf
           ghi     rf                  ; propagate carry
           adi     0
           phi     rf
           shl                         ; now ring shift left
           glo     rf
           shlc
           plo     rf
           ghi     rf
           shlc
           phi     rf
           inc     r2                  ; [RLA] point to NVR address
           ldn     r2                  ; retrieve it
           adi     1                   ; point to next byte
           str     r2                  ; store again
           glo     rc                  ; get end
           sm                          ; see if at end
           lbnz    nvr_chklp           ; loop back if not
           irx                         ; [RLA] restore rc.0
           ldx                         ; [RLA] ...
           plo     rc                  ; [RLA] ...
           sep     sret                ; and return

; ************************
; *** Set NVR checksum ***
; *** RF - checkshum   ***
; ************************
set_chk:   glo     rc                  ; save consumed register
           stxd
           sep     scall               ; get size of nvram
           dw      rtctest
           adi     8eh-2               ; [RLA]
           plo     rc
           str     r2                  ; store for out
           out     NVR_SELECT          ; write register selection port
           dec     r2                  ; compensate for increment
           ghi     rf                  ; get high of checksum
           str     r2                  ; prepare for out
           out     NVR_DATA            ; write to NVRAM
           dec     r2                  ; [RLA] compensate for out
           inc     rc                  ; next position
           glo     rc                  ; setup address
           str     r2                  ; store for out
           out     NVR_SELECT          ; write register selection port
           dec     r2                  ; compensate for increment
           glo     rf                  ; get low of checksum
           str     r2                  ; prepare for out
           out     NVR_DATA            ; write to NVRAM
           ldx
           plo     rc
           sep     sret                ; and return

; ******************************
; *** Get NVR checksum       ***
; *** Returns: RD - checksum ***
; ******************************
get_chk:   glo     rc                  ; save consumed register
           stxd    
           sep     scall               ; get size of nvram
           dw      rtctest
           adi     8eh-2                 ; [RLA] setup address
           plo     rc    
           str     r2                  ; store for out
           out     NVR_SELECT          ; compensate for inc
           dec     r2                  ; [RLA] correct for out instruction
           inp     NVR_DATA            ; read NVR 
           phi     rd                  ; put into RD.1
           inc     rc                  ; move to next address
           glo     rc                  ; get checksum address
           str     r2                  ; store for out
           out     NVR_SELECT          ; compensate for inc
           dec     r2                  ; [RLA] correct for out instruction
           inp     NVR_DATA            ; read NVR 
           plo     rd                  ; put into RD.0
           irx                         ; recover consumed register
           ldx     
           plo     rc
           sep     sret                ; return to caller

; ****************************************************
; *** Output 2 digit decimal number with leading 0 ***
; *** D - value to output                          ***
; *** RF - buffer to write value to                ***
; ****************************************************
intout2:
        str     r2              ; save value for a moment
        ldi     0               ; setup count
        plo     re
        ldn     r2              ; retrieve it
intout2lp:
        smi     10              ; subtract 10
        lbnf    intout2go       ; jump if too small
        inc     re              ; increment tens
        lbr     intout2lp       ; and keep looking
intout2go:
        adi     10              ; make positive again
        str     r2              ; save units
        glo     re              ; get tens
        adi     '0'             ; convert to ascii
        str     rf              ; store into buffer
        inc     rf
        ldn     r2              ; recover units
        adi     '0'             ; convert to ascii
        str     rf              ; and store into buffer
        inc     rf
        sep     sret            ; return to caller

; ************************************
; *** Convert packed date to ascii ***
; *** RD - pointer to packed date  ***
; *** RF - pointer to buffer       ***
; ************************************
dttoas: lda     rd              ; retrieve month
        sep     scall           ; output it
        dw      intout2
        ldi     '/'             ; now a slash
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; retrieve day
        sep     scall           ; output it
        dw      intout2
        ldi     '/'             ; now a slash
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; [RLA] get year
        stxd                    ; save contents of RD
        adi     180             ; add base of 1972
        plo     rd
        ghi     rd              ; save high byte
        stxd
        ldi     7               ; high byte of 1972
        adci    0               ; propagate the carry
        phi     rd
        sep     scall           ; output the year
        dw      f_uintout
        ldi     0               ; write terminator
        str     rf
        irx                     ; recover rd
        ldxa
        phi     rd
        ldx
        plo     rd
        sep     sret            ; and return

; ************************************
; *** Convert packed time to ascii ***
; *** RD - pointer to packed time  ***
; *** RF - pointer to buffer       ***
; ************************************
tmtoas: ldi     ' '             ; space after year
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; get hour
        sep     scall           ; output it
        dw      intout2
        ldi     ':'             ; now a colon
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; get minutes
        sep     scall           ; output it
        dw      intout2
        ldi     ':'             ; now a colon
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; get seconds
        sep     scall           ; output it
        dw      intout2
        ldi     0
        str     rf              ; write terminator
        sep     sret            ; and return

; ************************************
; *** Convert ascii date to packed ***
; *** RF - pointer to ascii date   ***
; *** RA - destination address     ***
; ************************************
astodt: glo     rd              ; save consumed register
        stxd
        ghi     rd
        stxd
        sep     scall           ; convert first number
        dw      atoi
        glo     rd              ; get value
        smi     13              ; see if valid month
        lbdf    dterr           ; jump if invalid
        glo     rd              ; recover number
        str     ra              ; store into output
        inc     ra              ; and increment
        lda     rf              ; get next char
        smi     '/'             ; must be a slash
        lbnz    dterr
        sep     scall           ; get next number
        dw      atoi
        glo     rd              ; get value
        smi     32              ; check next number
        lbdf    dterr           ; jump if invalid
        glo     rd              ; recover value
        str     ra              ; and save in output
        inc     ra              ; and increment
        lda     rf              ; get next char
        smi     '/'             ; must be a slash
        lbnz    dterr           ; jump if not
        sep     scall           ; now convert year
        dw      atoi
        ghi     rd              ; get high byte
        lbnz    y4              ; jump if 4 digit year
        glo     rd              ; need to check y2k
        smi     72              ; need to check for y2k threshold
        lbnf    y2k             ; jump if 2000+
asdtend:
        str     ra              ; store into output
        inc     ra              ; move to next position
        adi     0               ; signal valid date
        lbr     get_rd          ; recover RD and return
y4:     glo     rd              ; subtract 1972
        smi     180
        lbr     asdtend         ; and store
y2k:    glo     rd              ; recover value
        adi     28              ; add in offset
        lbr     asdtend         ; and continue processing

; ************************************
; *** Convert ascii time to packed ***
; *** RF - pointer to ascii time   ***
; *** RA - destination address     ***
; ************************************
astotm: glo     rd              ; save consumed register
        stxd
        ghi     rd
        stxd
        sep     scall           ; move past any spaces
        dw      ltrim
        sep     scall           ; convert hours
        dw      atoi
        glo     rd              ; check if valid
        smi     24
        lbdf    dterr           ; jump if not
        glo     rd              ; store value into result
        str     ra
        inc     ra
        lda     rf              ; get next char
        smi     ':'             ; must be a colon
        lbnz    dterr           ; jump if not
        sep     scall           ; convert minutes
        dw      atoi
        glo     rd              ; check for valid
        smi     60
        lbdf    dterr           ; jump if not
        glo     rd              ; store value into result
        str     ra
        inc     ra
        lda     rf              ; get next char
        plo     re              ; keep a copy
        smi     ':'             ; check for colon
        lbz     is_sec          ; jump if seconds are provided
        glo     re              ; need to check for valid characters
        lbz     no_sec          ; jump if terminator
        smi     32              ; otherwise must be a space
        lbnz    dterr           ; jump on other chars to err
no_sec: ldi     0               ; set seconds to zero
tm_cont:str     ra
        adi     0               ; signal no error
get_rd: irx                     ; recover consumed register
        ldxa
        phi     rd
        ldx
        plo     rd
        sep     sret            ; and return
is_sec: sep     scall           ; convert seconds
        dw      atoi
        glo     rd              ; check for valid
        smi     60
        lbdf    dterr           ; jump if not valid
        glo     rd              ; store answer and return
        lbr     tm_cont
dterr:  smi     0               ; signal an error
        lbr     get_rd          ; recover RD and return
#endif  ;; "#ifdef NVR" ...

#ifdef EIDE
; ********************************
; *** Select master ide device ***
; ********************************
master:    sep     scall               ; wait til drive ready
           dw      waitrdy
           lbdf    sel_err             ; jump if timedout
           sex     r3                  ; setup for immediate outs
           out     IDE_SELECT          ; select device register
           db      6
           out     IDE_DATA            ; now output code for master
           db      0
           sex     r2                  ; restore x register
           adi     0                   ; signal no error
           sep     sret                ; and return

sel_err:   smi     0                   ; signal an error
           sep     sret                ; and return
; *******************************
; *** Select slave ide device ***
; *******************************
slave:     sep     scall               ; wait til drive ready
           dw      waitrdy
           lbdf    sel_err             ; jump if timedout
           sex     r3                  ; setup for immediate outs
           out     IDE_SELECT          ; select device register
           db      6
           out     IDE_DATA            ; now output code for slave
           db      010h
           sex     r2                  ; restore x register
           adi     0                   ; signal no error
           sep     sret                ; and return

; ************************************
; *** get ide identity information ***
; *** RF - pointer to buffer       ***
; *** RD.0 = 0 master              ***
; ***      = 1 slave               ***
; *** Returns: DF=1 drive error    ***
; ************************************
ide_ident: glo     rf                  ; save buffer position
           stxd
           ghi     rf
           stxd
           sep     scall               ; read drive data
           dw      ide_id
           irx                         ; recover buffer
           ldxa
           phi     rf
           ldx
           plo     rf
           lbdf    sel_err             ; jump if drive error
           lbr     swap                ; perform byte swaps
          
ide_id:    glo     rd
           lbz     id_master
           sep     scall               ; select slave drive
           dw      slave
           lbdf    sel_err             ; jump on error
           lbr     ident_go
id_master: sep     scall               ; select master
           dw      master
           lbdf    sel_err             ; jump on error
ident_go:  glo     rc                  ; save consumed registers
           stxd
           ghi     rc
           stxd
           sep     scall               ; wait for drive to be ready
           dw      waitrdy
           lbdf    sel_err             ; jump on timeout
           sex     r3                  ; setup for outs
           out     IDE_SELECT          ; select command port
           db      7
           out     IDE_DATA            ; issue get identity command
           db      0ech
           sex     r2                  ; reset stack
           sep     scall               ; wait for DRQ
           dw      drqloop
           lbr     ide_read            ; read drive data

swap:      ldi     0                   ; setup counter
           plo     rc
swap_lp:   lda     rf                  ; get first byte
           plo     re                  ; keep a copy
           ldn     rf                  ; get second byte
           phi     rc                  ; keep a copy
           glo     re                  ; get first byte
           str     rf                  ; write into 2nd position
           dec     rf                  ; point back to first byte
           ghi     rc                  ; get 2nd byte
           str     rf                  ; write into first position
           inc     rf                  ; point to next word
           inc     rf
           dec     rc                  ; decrement count
           glo     rc                  ; see if done
           lbnz    swap_lp             ; loop back if not
           sep     sret                ; otherwise return to caller

; *****************************************
; *** Determine size of hard drive      ***
; *** RD.0 = 0 - master drive           ***
; *** RD.0 = 1 - slave drive            ***
; *** Returns: RF - size of drive in MB ***
; ***               0=no drive          ***
; *****************************************
ide_size:  glo     rd                  ; get selected drive
           lbz     sz_master           ; jump if master is selected
           sep     scall               ; select slave drive
           dw      slave
           lbr     size_go             ; then continue
sz_master: sep     scall               ; select master drive
           dw      master
size_go:   sex     r3                  ; setup for immediate out
           out     IDE_SELECT          ; select status register
           db      07h
           sex     r2                  ; reset stack
           ldi     0                   ; setup for failure count
           plo     rc
           phi     rc
rdyloopz:  inp     IDE_DATA            ; read status port
           ani     0c0h                ; mask for BSY and RDY
           smi     040h                ; want only RDY bit
           lbz     sz_ready            ; jump if drive is ready
           dec     rc                  ; decrement timetout
           glo     rc                  ; check if timeout occurred
           lbnz    rdyloopz            ; jump if not
           ghi     rc
           lbnz    rdyloopz
           ldi     0                   ; signify no drive
           phi     rf
           plo     rf
           sep     sret                ; and return
sz_ready:  sex     r3                  ; issued commands to perform an ident
           out     IDE_SELECT          ; select command register
           db      07h
           out     IDE_DATA            ; issue identify command
           db      0ech
           sex     r2                  ; pont x back to stack
           sep     scall               ; wait for DRQ
           dw      drqloop
           sex     r3                  ; now setup data register
           out     IDE_SELECT
           db      0
           sex     r2                  ; x back to stack
           ldi     120                 ; need to throw away 120 bytes
           plo     rc
size_lp1:  inp     IDE_DATA            ; read byte from drive
           dec     rc                  ; decrement count
           glo     rc                  ; see if done
           bnz     size_lp1            ; loop back if not
           inp     IDE_DATA            ; read 4 bytes into r8:r7
           plo     r7
           plo     rf                  ; also into RF
           inp     IDE_DATA
           phi     r7
           phi     rf
           inp     IDE_DATA
           plo     r8
           inp     IDE_DATA
           phi     r8
           ldi     1                   ; need to read 388 more bytes
           phi     rc
           ldi     132
           plo     rc
size_lp2:  inp     IDE_DATA            ; read byte
           dec     rc                  ; decrement count
           glo     rc                  ; see if done
           bnz     size_lp2
           ghi     rc                  ; check high byte
           bnz     size_lp2
           ghi     r8                  ; save R8
           stxd
           glo     r8
           stxd
           ghi     rf                  ; need 11 shift to convert to MB
           plo     rf
           glo     r8
           phi     rf
           ghi     r8
           plo     r8                  ; 8 shift are now done
           ldi     3                   ; need 3 more
           plo     rc
size_lp3:  glo     r8                  ; shift whole number right by 1
           shr
           plo     r8
           ghi     rf
           shrc
           phi     rf
           glo     r7
           shrc
           plo     rf
           dec     rc                  ; decrement count
           glo     rc                  ; see if done
           bnz     size_lp3            ; jump if not
           irx                         ; recover R8
           ldxa
           plo     r8
           ldx
           phi     r8
           sep     sret                ; and return

; ***********************************
; *** Check for valid boot loader ***
; ***********************************
btcheck:   ldi     1                   ; point to boot code
           phi     rf
           ldi     0
           plo     rf
           ldi     041h                ; number of bytes to check
           plo     rc
           ldi     0                   ; setup initial value
           plo     rd
           sex     rf                  ; point X to boot code
btchk_lp:
           glo     rd                  ; get value
           add                         ; add in next byte
           irx                         ; move pointer
           shl                         ; shift high byte into DF
           shr                         ; move bits back
           shlc                        ; ring shift now done
           plo     rd                  ; save value
           dec     rc                  ; decrement byte count
           glo     rc                  ; see if done
           bnz     btchk_lp            ; loop back if not
           sex     r2                  ; point X back to stack
           glo     rd                  ; get number
           smi     060h                ; check against check value
           lbnz    err                 ; jump on mismatch to error
           adi     0                   ; signal good
           sep     sret                ; and return
#endif  ;; "#ifdef EIDE" ...


; ****************************************************************
; * [RLA] Extended BIOS vectors ...                              *
; * [RLA]   This is probably obvious, but DON'T CHANGE THE ORDER *
; * [RLA] OF THESE VECTORS!!!                                    *
; ****************************************************************
           org     BASE+0800h
f_bread:   lbr     read
f_btype:   lbr     type
f_btest:   lbr     brktest
#ifdef UART
f_utype:   lbr     e2k_utype    ; [RLA] type console character via UART
f_uread:   lbr     e2k_uread    ; [RLA] read console character via UART
f_utest:   lbr     e2k_utest    ; [RLA] test UART for character available
f_usetbd:  lbr     e2k_stbd     ; [RLA] set UART baud rate and character format
#else
f_utype:   lbr     err
f_uread:   lbr     err
f_utest:   lbr     err
f_usetbd:  lbr     err
#endif
#ifdef NVR
f_gettod:  lbr     e2k_gtod     ; [RLA] get RTC time and date
f_settod:  lbr     e2k_stod     ; [RLA] set RTC time and date
f_rdnvr:   lbr     e2k_rdnvr    ; [RLA] read NVR data
f_wrnvr:   lbr     e2k_wrnvr    ; [RLA] write NVR data
#else
f_gettod:  lbr     err
f_settod:  lbr     err
f_rdnvr:   lbr     err
f_wrnvr:   lbr     err
#endif
#ifdef EIDE
f_idesize: lbr     ide_size     ; [RLA] return the size of attached IDE drive
f_ideid:   lbr     ide_ident    ; [RLA] return the manufacturer of IDE drive
#else
f_idesize: lbr     err
f_ideid:   lbr     err
#endif
#ifdef NVR
f_dttoas:  lbr     dttoas       ; [RLA] convert date to ASCII
f_tmtoas:  lbr     tmtoas       ; [RLA] convert time to ASCII
f_rtctest: lbr     rtctest      ; [RLA] test if NVR/RTC present
f_astodt:  lbr     astodt       ; [RLA] parse ASCII date
f_astotm:  lbr     astotm       ; [RLA] parse ASCII time
f_nvrcchk: lbr     nvr_chk      ; [RLA] compute NVR checksum
#else
f_dttoas:  lbr     err
f_tmtoas:  lbr     err
f_rtctest: lbr     err
f_astodt:  lbr     err
f_astotm:  lbr     err
f_nvrcchk: lbr     err
#endif

; *** rf - pointer to ascii string
; *** returns: rf - first non-numeric character
; ***          RD - number
; ***          DF = 1 if first character non-numeric 
atoi:      ldi     0                   ; clear answer
           phi     rd
           plo     rd
           plo     re                  ; signify positive number
           ldn     rf                  ; get first value
           sep     scall               ; check if numeric
           dw      isnum
           bdf     atoicnt             ; jump if so
           xri     '-'                 ; check for minus
           bz      atoicnt             ; jump if so
           smi     0                   ; signal number error
           sep     sret                ; return to caller
atoicnt:   ldn     rf                  ; get first bytr
           xri     '-'                 ; check for negative
           bnz     atoilp              ; jump if not negative
           ldi     1                   ; signify negative number
           plo     re
           inc     rf                  ; move past - sign
atoilp:    ldn     rf                  ; get byte from input
           sep     scall               ; check for number
           dw      isnum
           lbnf    atoidn              ; jump if not
           ghi     rd                  ; make a copy for add
           stxd
           glo     rd                  ; multiply by 2
           stxd                        ; TOS now has copy of number
           sep     scall               ; multiply by 2
           dw      mul2
           sep     scall               ; multiply by 4
           dw      mul2
           irx                         ; point to adds
           glo     rd                  ; multiply by 5 (add TOS)
           add
           plo     rd
           irx                         ; point to msb
           ghi     rd
           adc
           phi     rd
           sep     scall               ; multiply by 10
           dw      mul2
           lda     rf                  ; get byte from buffer
           smi     '0'                 ; convert to binary
           str     r2                  ; prepare for addition
           glo     rd                  ; add in new digit
           add
           plo     rd
           ghi     rd
           adci    0
           phi     rd
           br      atoilp              ; loop back for next character
atoidn:    adi     0                   ; signal valid number
           sep     sret                ; return to caller
mul2:      glo     rd                  ; multiply number by 2
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           sep     sret                ; and return

; **** convert binary number to ascii
; **** RD - number to convert
; **** RF - buffer to store
; **** Returns: RF - last postion+1
uintout:   br      positive
intout:    sex     r2                  ; point X to stack
           ghi     rd                  ; get high of number
           ani     128                 ; mask all bit sign bit
           bz      positive            ; jump if number is positive
           ldi     '-'                 ; need a minus sign
           str     rf                  ; store into output
           inc     rf
           glo     rd                  ; get low byte
           str     r2                  ; store it
           ldi     0                   ; need to subtract from 0
           sm
           plo     rd                  ; put back
           ghi     rd                  ; get high byte
           str     r2                  ; place into memory
           ldi     0                   ; still subtracting from zero
           smb     
           phi     rd                  ; and put back 
positive:  glo     r7                  ; save consumed registers
           stxd
           ghi     r7
           stxd
           glo     r8                  ; save consumed registers
           stxd
           ghi     r8
           stxd
           glo     r9                  ; save consumed registers
           stxd
           ghi     r9
           stxd
           ldi     high numbers        ; point to numbers
           phi     r9
           ldi     low numbers
           plo     r9
           lda     r9                  ; get first division
           phi     r7
           lda     r9
           plo     r7
           ldi     0                   ; leading zero flag
           stxd                        ; store onto stack
nxtiter:   ldi     0                   ; star count at zero
           plo     r8                  ; place into low of r8
divlp:     glo     r7                  ; get low of number to subtrace
           str     r2                  ; place into memory
           glo     rd                  ; get low of number
           sm                          ; subtract
           phi     r8                  ; place into temp space
           ghi     r7                  ; get high of subtraction
           str     r2                  ; place into memory
           ghi     rd                  ; get high of number
           smb                         ; perform subtract
           bnf     nomore              ; jump if subtraction was too large
           phi     rd                  ; store result
           ghi     r8
           plo     rd
           inc     r8                  ; increment count
           br      divlp               ; and loop back
nomore:    irx                         ; point back to leading zero flag
           glo     r8
           bnz     nonzero             ; jump if not zero
           ldn     r2                  ; get flag
           bnz     allow0              ; jump if no longer zero
           dec     r2                  ; keep leading zero flag
           br      findnxt             ; skip output
allow0:    ldi     0                   ; recover the zero
nonzero:   adi     30h                 ; convert to ascii
           str     rf                  ; store into buffer
           inc     rf
           ldi     1                   ; need to set leading flag
           stxd                        ; store it
findnxt:   dec     r7                  ; subtract 1 for zero check
           glo     r7                  ; check for end
           bz      intdone             ; jump if done
           lda     r9                  ; get next number
           phi     r7
           lda     r9
           plo     r7
           smi     1                   ; see if at last number
           bnz     nxtiter             ; jump if not 
           irx                         ; set leading flag
           ldi     1
           stxd
           br      nxtiter
intdone:   irx                         ; put x back where it belongs
           irx                         ; recover consumed registers
           ldxa
           phi     r9
           ldxa
           plo     r9
           ldxa
           phi     r8
           ldxa
           plo     r8
           ldxa
           phi     r7
           ldx
           plo     r7
           sep     sret                ; return to caller



           org     BASE+0900h
#ifdef MCHIP
buffer:    equ     0fc00h
#else
buffer:    equ     03
#endif
minimon:   ldi     high start          ; setup main pc
           phi     r6
           ldi     low start
           plo     r6
#ifdef MCHIP
           ldi     0fdh                ; setup stack
           phi     r2
           ldi     0ffh
           plo     r2
#else
           ldi     0                   ; setup stack
           phi     r2
           ldi     0ffh
           plo     r2
#endif
           sex     r2
           lbr     f_initcall
start:     sep     scall               ; initialize baud setting
           dw      f_setbd

mainlp:    ldi     high prompt         ; get address of prompt
           phi     rf
           ldi     low prompt
           plo     rf
           sep     scall               ; display prompt
           dw      f_msg
           sep     scall
           dw      loadbuf
           sep     scall               ; get input from user
           dw      f_input
           sep     scall
           dw      docrlf
           sep     scall
           dw      loadbuf
           lda     rf                  ; get first byte
           phi     rc                  ; save it
           sep     scall               ; obtain address
           dw      f_hexin
           ghi     rd                  ; move to address var
           phi     ra
           glo     rd
           plo     ra

           ghi     rc                  ; retrieve command
           smi     33
           bz      storesp
#ifdef ANYROM
           smi     14                  ; check for / return to os
           lbz     EXIT_ADDR           ; ROM exit vector        
           smi     14                  ; look for copy command
#else            
           smi     28                  ; look for copy command
#endif
           bz      copy                ; jump if found
           smi     2
           bz      examine
           smi     1
           bnz     mainlp
run:       ghi     ra                  ; move to address var
           phi     r0
           glo     ra
           plo     r0
           sex     r0
           sep     r0
           
examine:   ldi     8                   ; set count to 128 bytes
           plo     rc
exloop1:   ghi     ra                  ; get address
           phi     rd                  ; transfer for output
           glo     ra
           plo     rd
           sep     scall
           dw      loadbuf
           sep     scall               ; put address into output
           dw      f_hexout4
           ldi     ':'                 ; want a colon
           str     rf
           inc     rf
           ldi     16                  ; 16 bytes per line
           plo     rb                  ; put into secondary counter
exloop2:   ldi     ' '                 ; output a space
           str     rf
           inc     rf
           lda     ra                  ; get next byte from memory
           plo     rd                  ; prepare for output
           sep     scall               ; convert for output
           dw      f_hexout2
           dec     rb                  ; decrment line count
           glo     rb                  ; get count
           bnz     exloop2             ; loop back if not done
           ldi     0                   ; need terminator
           str     rf
           sep     scall
           dw      loadbuf
           sep     scall               ; output the line
           dw      f_msg
           sep     scall
           dw      docrlf
           dec     rc                  ; decrement line count
           glo     rc                  ; get count
           bnz     exloop1             ; loop back if not all lines printed
           br      mainlp              ; return to main loop

storesp:   ldn     rf                  ; get byte from input
           bz      mainlp              ; jump if found
           smi     33                  ; check for less than space
           bdf     storec              ; jump if not space
           inc     rf                  ; point to next character
           br      storesp             ; and keep moving past spaces
storec:    sep     scall               ; convert next number
           dw      f_hexin
           glo     rd                  ; get converted byt
           str     ra                  ; store into memory
           inc     ra                  ; point to next position
           br      storesp             ; and do next character

copy:      ghi     ra                  ; move source
           phi     r8
           glo     ra
           plo     r8
           sep     scall               ; move past spaces
           dw      f_ltrim
           sep     scall               ; get destination address
           dw      f_hexin
           ghi     rd                  ; transfer to r9
           phi     r9
           glo     rd
           plo     r9
           sep     scall               ; move past spaces
           dw      f_ltrim
           sep     scall               ; get source address
           dw      f_hexin
           ghi     rd                  ; transfer to rc
           phi     rc
           glo     rd
           plo     rc
movelp:    ghi     rc                  ; check for zero
           bnz     domove
           glo     rc
           bnz     domove
           br      done
domove:    lda     r8
           str     r9
           inc     r9
           dec     rc
           br      movelp
done:      br      mainlp

loadbuf:   ldi     high buffer
           phi     rf
           ldi     low buffer
           plo     rf
           sep     sret

docrlf:    ldi     high crlf
           phi     rf
           ldi     low crlf
           plo     rf
           sep     scall
           dw      f_msg
           sep     sret

prompt:    db      '>',0
crlf:      db      10,13,0

; ***************************************
; *** Type inline message             ***
; ***************************************
typeinmsg: lda     r6                  ; load byte from message
           lbz     return              ; return if last byte
           sep     scall               ; call type routine
           dw      f_tty
           br      typeinmsg           ; loop until a zero found

; *************************************
; *** Check if character is numeric ***
; *** D - char to check             ***
; *** Returns DF=1 if numeric       ***
; ***         DF=0 if not           ***
; *************************************
isnum:     plo     re                  ; save a copy
           smi     '0'                 ; check for below zero
           bnf     fails               ; jump if below
           smi     10                  ; see if above
           bdf     fails               ; fails if so
passes:    smi     0                   ; signal success
           lskp
fails:     adi     0                   ; signal failure
           glo     re                  ; recover character
           sep     sret                ; and return

err:       smi     0                   ; signal an error
           sep     sret                ; and return

           org     BASE+0a00h
; ***************************************************************
; *** Function to convert hex input characters to binary      ***
; *** RF - Pointer to characters                              ***
; *** Returns - RF - First character that is not alphanumeric ***
; ***           RD - Converted number                         ***
; ***************************************************************
hexin:     ldi     0                   ; set initial total
           phi     rd
           plo     rd
tobinlp:   lda     rf                  ; get input character
           smi     '0'                 ; convert to binary
           bnf     tobindn             ; jump if termination
           stxd
           ani     0f0h                ; check for alpha
           irx                         ; point back
           bz      isnumeric
           ldx                         ; recover byte
           smi     49                  ; see if lowercase
           bnf     hexgo
           ldx                         ; get byte
           smi     32                  ; convert to uppercase
           br      hexgo2              ; and continue
hexgo:     ldx                         ; recover byte
hexgo2:    smi     7                   ; offset
           br      tobingo             ; and continue
isnumeric: ldx                         ; recover byte
           smi     10                  ; check for end of numbers
           bdf     tobindn             ; jump if end
           ldx                         ; recover byte
tobingo:   stxd                        ; save number
           smi     16                  ; check for valid range
           bnf     tobingd             ; jump if good
           irx                         ; remove number from stack
           br      tobindn
tobingd:   ldi     4                   ; need to multiply by 16
tobinglp:  stxd
           glo     rd                  ; multiply by 2
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           irx
           ldi     1
           sd
           bnz     tobinglp
           irx                         ; point to new number
           glo     rd                  ; and add to total
           add
           plo     rd
           ghi     rd
           adci    0
           phi     rd
           br      tobinlp             ; loop back for next character
tobindn:   dec     rf                  ; move back to terminating character
           sep     sret                ; return to caller

; *********************************************
; *** Convert a binary number to hex output ***
; *** RD - Number to convert                ***
; *** RF - Buffer for output                ***
; *** Returns: RF - next buffer position    ***
; ***          RD - consumed                ***
; *********************************************
hexout2:   glo     rd                  ; move low byte to high
           phi     rd
           ldi     2                   ; 2 nybbles to display
           lskp                        ; skip over the 4
hexout4:   ldi     4                   ; 4 nybbles to display
hexoutlp:  stxd                        ; save the count
           ldi     0                   ; zero the temp var
           plo     re
           ldi     4                   ; perform 4 shift
hexoutl2:  stxd                        ; save count
           glo     rd                  ; perform shift
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           glo     re
           shlc
           plo     re
           irx                         ; point back to count
           ldi     1                   ; need to decrement it
           sd
           bnz     hexoutl2            ; jump if more shifts needed
           glo     re                  ; get nybble
           smi     10                  ; compare to 10
           bdf     hexoutal            ; jump if alpha
           glo     re                  ; get value
           adi     30h                 ; convert to ascii
hexoutl3:  str     rf                  ; store value into buffer
           inc     rf
           irx                         ; point to count
           ldi     1                   ; need to subtract 1 from it
           sd
           bnz     hexoutlp            ; loop if not done
           sep     sret                ; return to caller
hexoutal:  glo     re                  ; get value
           adi     55                  ; convert to ascii
           br      hexoutl3            ; and continue


         sep     r3                    ; jump to called routine
call:    plo     re                    ; Save D
         ghi     r6                    ; save last R[6] to stack
         sex     r2
         stxd
         glo     r6
         stxd
         ghi     r3                    ; copy R[3] to R[6]
         phi     r6
         glo     r3
         plo     r6
         lda     r6                    ; get subroutine address
         phi     r3                    ; and put into r3
         lda     r6
         plo     r3
         glo     re                    ; recover D
         br      call-1                ; transfer control to subroutine

         sep     r3                    ; transfer control back to coller
ret:     plo     re                    ; Save D
         ghi     r6                    ; copy R[6] to R[3]
         phi     r3
         glo     r6
         plo     r3
         sex     r2
         irx                           ; point to old R[6]
         ldxa
         plo     r6
         ldx
         phi     r6
         glo     re
         br      ret-1                 ; and perform return to caller

; ********************************
; *** See if D is alphabetic   ***
; *** Returns DF=0 - not alpha ***
; ***         DF=1 - is alpha  ***
; ********************************
isalpha:   plo     re                  ; save copy of do
           smi     'A'                 ; check uc letters
           lbnf    fails               ; jump if below A
           smi     27                  ; check upper range
           lbnf    passes              ; jump if valid
           glo     re                  ; recover character
           smi     'a'                 ; check lc letters
           lbnf    fails               ; jump if below A
           smi     27                  ; check upper range
           lbnf    passes              ; jump if valid
           lbr     fails

; **********************************
; *** check D if hex             ***
; *** Returns DF=1 - hex         ***
; ***         DF=0 - non-hex     ***
; **********************************
ishex:     sep     scall               ; see if it is numeric
           dw      isnum
           plo     re                  ; keep a copy
           lbdf    passes              ; jump if it is numeric
           smi     'A'                 ; check for below uppercase a
           lbnf    fails               ; value is not hex
           smi     6                   ; check for less then 'G'
           lbnf    passes              ; jump if so
           glo     re                  ; recover value
           smi     'a'                 ; check for lowercase a
           lbnf    fails               ; jump if not
           smi     6                   ; check for less than 'g'
           lbnf    passes              ; jump if so
           lbr     fails

sdread:    ani     1                   ; want only 0 or 1 in device
           ori     0eh                 ; ide LBA mode
           shl                         ; shift to high nybble
           shl
           shl
           shl
           str     r2                  ; need to combine with R8.1
           ghi     r8
           ani     00fh                ; strip high nybble
           or                          ; or with device setting
           phi     r8                  ; back into r8.1
           lbr     rdide               ; now standard read

sdwrite:   ani     1                   ; want only 0 or 1 in device
           ori     0eh                 ; ide LBA mode
           shl                         ; shift to high nybble
           shl
           shl
           shl
           str     r2                  ; need to combine with R8.1
           ghi     r8
           ani     00fh                ; strip high nybble
           or                          ; or with device setting
           phi     r8                  ; back into r8.1
           lbr     wrtide              ; now standard write

           org     BASE+0b00h
resetide:  sep     scall               ; wait til drive ready
           dw      waitrdy
           bdf     ide_err             ; jump if timout
           glo     rd                  ; get selected drive
           shr                         ; shift over 4 bits
           shr
           shr
           shr
           stxd                        ; write drive select to stack
           ldi     6                   ; select device register
           str     r2                  ; write to stack
           out     IDE_SELECT          ; write select port
           out     IDE_DATA            ; write device code
           dec     r2                  ; point back
           sex     r3                  ; setup for immediate outs

           out     IDE_SELECT          ; select interrupt port
           db      00eh
           out     IDE_DATA            ; function to perform soft reset
           db      4
           out     IDE_DATA            ; and clear it
           db      0
           sex     r2                  ; reset x to stack
           sep     scall               ; wait til drive ready
           dw      waitrdy
           bdf     ide_err             ; jump if timout
           sex     r3                  ; setup for immediate outs
           out     IDE_SELECT          ; select feature register
           db      1
           out     IDE_DATA            ; enable 8 bit mode
           db      1
           out     IDE_SELECT          ; select command register
           db      7
           out     IDE_DATA            ; command to set features
           db      0efh


           sex     r2                  ; point X back to stack
           sep     scall               ; wait til drive ready
           dw      waitrdy
           bdf     ide_err             ; jump if timout
ide_good:  adi     0                   ; signal no error
           sep     sret                ; return to caller
ide_err:   smi     0                   ; signal error occurred
           sep     sret
           
wrtide:    glo     rc                  ; save consumed registers
           stxd
           ghi     rc
           stxd
           sep     scall               ; call wait til ready
           dw      waitrdy
           bdf     ide_fail            ; jump on timeout error
           ldi     030h                ; command for sector write
           sep     scall               ; now setup command
           dw      wrtcmd
           bdf     ide_fail            ; jump if error occurred
           ldi     2                   ; high byte of 512
           phi     rc                  ; place into count
           ldi     0                   ; low byte of 512
           plo     rc                  ; place into low of count
           ldi     0                   ; need data register
           str     r2                  ; place on stack
           out     IDE_SELECT          ; select data register
           dec     r2                  ; move pointer
           sex     rf                  ; set data pointer
wrtloop:   out     IDE_DATA            ; write to ide controller
           dec     rc                  ; decrement byte count
           glo     rc                  ; check for completion
           bnz     wrtloop             ; jump if not
           ghi     rc                  ; need to check high byte
           bnz     wrtloop             ; jump if more to go
           sex     r2                  ; point x to stack
           sep     scall               ; call wait til ready
           dw      waitrdy
           bdf     ide_fail            ; jump if error occurred
           adi     0                   ; signal no error
           ldi     0                   ; no errors in D
ideret:    plo     re
           irx                         ; recover consumed registers
           ldxa
           phi     rc
           ldx
           plo     rc
           glo     re
           sep     sret                ; and return to caller
ide_fail:  sex     r3                  ; setup for immediate out
           out     IDE_SELECT          ; select error register
           db      1
           sex     r2                  ; point X back to stack
           inp     IDE_DATA            ; read error register
           smi     0                   ; signal an error
           br      ideret              ; and return

rdide:     glo     rc                  ; save consumed registers
           stxd
           ghi     rc
           stxd
           sep     scall               ; call wait til ready 
           dw      waitrdy
           bdf     ide_fail            ; jump on timeout error
           ldi     020h                ; command for sector read
           sep     scall               ; now setup command
           dw      wrtcmd
           bdf     ide_fail            ; jump if error occurred
ide_read:  ldi     2                   ; high byte of 512
           phi     rc                  ; place into count
           ldi     0                   ; lo byte of 512
           plo     rc                  ; place into low of count
           str     r2                  ; place on stack
           out     IDE_SELECT          ; select data register
           dec     r2                  ; move pointer
           sex     rf                  ; set data pointer
rdloop:    inp     IDE_DATA            ; read from ide controller
           inc     rf                  ; point to next position
           dec     rc                  ; decrement byte count
           glo     rc                  ; check for completion
           bnz     rdloop              ; jump if not
           ghi     rc                  ; need to check high byte
           bnz     rdloop              ; jump if more to go
           ldi     0                   ; signify read complete
           sex     r2
           br      ideret              ; return to caller
          
waitrdy:   ldi     07h                 ; need status register
           str     r2                  ; store onto stack
           out     IDE_SELECT          ; write ide selection port
           dec     r2                  ; point x back to free spot
;[RLA]   Note that the whole rdyloop thing takes about 24 machine cycles or 192
;[RLA] clocks. The timeout here is 65,536 iterations which, at 2MHz, is about
;[RLA] 6 seconds...
           ldi     0                   ; setup timeout
           plo     rc
           phi     rc
rdyloop:   dec     rc                  ; decrement timeout
           glo     rc                  ; check for end
           bnz     rdy_go
           ghi     rc
           bnz     rdy_go
ideerror:  sex     r3                  ; setup for immediate out
           out     IDE_SELECT          ; seelct error register
           db      1
           sex     r2                  ; set X back to stack
           inp     IDE_DATA            ; read error register into D
           smi     0                   ; signal error occurred
           sep     sret                ; return to caller
rdy_go:    inp     IDE_DATA            ; read status port
           shr                         ; shift error bit
           bdf     ideerror            ; jump if error occurred
           shlc                        ; shift it back
           ani     0c0h                ; mask for BSY and RDY
           smi     040h                ; want only RDY bit
           bnz     rdyloop             ; loop back until drive is ready
           ldn     r2                  ; get status byte
           adi     0                   ; signal good
           sep     sret                ; return to caller
; RF will point to wrtcmd, which is next needed after first waitrdy
wrtcmd:    stxd                        ; write passed command to stack
           ldi     7                   ; command register
           stxd                        ; write to stack
           ghi     r8                  ; get device
           ori     0e0h                ; add IDE bits
           stxd                        ; write to stack
           ldi     6                   ; head/device register
           stxd                        ; write to stack
           glo     r8                  ; get high of lba
           stxd                        ; write to stack
           ldi     5                   ; cylinder high register
           stxd                        ; write to stack
           ghi     r7                  ; get mid of lba
           stxd                        ; write to stack
           ldi     4                   ; cylinder lo register
           stxd                        ; write to stack
           glo     r7                  ; get lo of lba
           stxd                        ; write to stack
           ldi     3                   ; sector start register
           stxd                        ; write to stack
           ldi     1                   ; read one sector
           stxd                        ; write to stack
           ldi     2                   ; sector count register register
           stxd                        ; write to stack
           ghi     r8                  ; get device
           stxd                        ; write to stack
           ldi     6                   ; head/device register
           str     r2                  ; write to stack
           ldi     7                   ; 7 sets to output
wrtcmd2:   out     IDE_SELECT
           out     IDE_DATA
           smi     1                   ; subtract 1
           bnz     wrtcmd2             ; loop back if not done
           dec     r2                  ; point back to free register
drqloop:   inp     IDE_DATA            ; read status register
           shr                         ; get error bit
           bdf     ideerror            ; jump if error
           shlc                        ; restore value
           shl                         ; check BSY bit
           bdf     drqloop             ; jump if set
           shrc                        ; restore value
           ani     8                   ; mask for DRQ bit
           bz      drqloop             ; loop until found
           sep     sret                ; return to caller
; the branch to beforerdy, allows us to use waitrdy again

           org     BASE+0c00h
           sep     r3
delay:     ghi     re                  ; get baud constant
           shr                         ; remove echo flag
           plo     re                  ; put into counter
           sex     r2                  ; waste a cycle
delay1:    dec     re                  ; decrement counter
           glo     re                  ; get count
           bz      delay-1             ; return if zero
           br      delay1              ; otherwise keep going

timalc:
;[RLA]   If the Elf2K 80 column video card and PS/2 keyboard are in use, then
;[RLA] this is completely a no-op.  
#ifdef VIDEO
           ghi     re        ; [RLA] see if the VT1802 is active
           ani     0feh      ; [RLA] mask out the local echo bit
           xri     0feh      ; [RLA] BAUD.1 == 0xFE means VT1802
           bnz     timal1    ; [RLA] no - continue with autobaud
           sep     sret      ; [RLA] yes - return right away
timal1:
#endif
           SERREQ
           glo     rb                  ; save consumed registesr
           stxd
           ghi     rb
           stxd
           glo     rc
           stxd
           ghi     rc
           stxd
           ldi     0                   ; zero counter 1
           phi     rc
           plo     rc
           phi     rb                  ; and counter 2
           plo     rb
#ifdef UART
           lbr     e2k_timalc          ; use elf2000 specific timalc
#endif
timalc_o:  SERP    $                   ; wait until start bit found
end_sb:    SERN    $                   ; wait until start bit found
           SERP    $
setbd1:    inc     rc
           sex     r2
           sex     r2
           SERN    setbd1              ; wait until another high
setbd2:    inc     rb
           sex     r2
           sex     r2
           SERP    setbd2              ; wait til the next low
setbd4:    glo     rb                  ; compare values
           shr                         ; quantize over small differences
           shr     
           str     r2
           glo     rc
           shr
           shr
           sm      
           bz      setbd3             ; jump if CR was entered
;           glo     rb
;           plo     rc
           ldi     1
           lskp    
setbd3:    ldi     0
           phi     rb
           glo     rc
           smi     4
           phi     re
           ghi     rb
           shr     
           ghi     re
           shlc    
           phi     re
timalc_rt: irx                         ; recover consumed registesr
           ldxa
           phi     rc
           ldxa
           plo     rc
           ldxa
           phi     rb
           ldx
           plo     rb
           sep     sret

type:      plo     re
           push    rf                  ; save consumed registers
           push    rd
           glo     re
           phi     rf
           ldi     9                   ; 9 bits to send
           plo     rf
           mov     rd,delay            ; point RD to delay routine
           ldi     0
           shr
sendlp:    bdf     sendnb              ; jump if no bit
           SERSEQ                      ; set output
           br      sendct
sendnb:    SERREQ                      ; reset output
           br      sendct
sendct:    sep     rd                  ; perform bit delay
           sex r2
           sex r2
           ghi     rf
           shrc
           phi     rf
           dec     rf
           glo     rf
           bnz     sendlp
           SERREQ                      ; set stop bits
           sep     rd
           sep     rd
           pop     rd                  ; recover consumed registers
           pop     rf
           sep     sret

#ifdef SERHDX
; [RLA]   If the console bit banged serial is half duplex (as is, for example,
; [RLA] the DS275 used in the ELF2K) then we can't echo input on a bit for bit
; [RLA] basis.  Istead, we have to read the entire character, wait for the stop
; [RLA] bit, and then transmit the whole character back to the terminal.  This
; [RLA] version of read: does precisely that ...
read:      glo     rf
           stxd
           ghi     rf
           stxd
           glo     rd
           stxd
           ghi     rd
           stxd
           ldi     9                   ; receive 9 bits (counting the START bit)
           plo     rf
           ldi     high delay
           phi     rd
           ldi     low delay
           plo     rd
           ghi     re                  ; first delay is half bit size
           phi     rf
           shr
           smi     01
           phi     re
           SERP    $                   ; wait for transmission
           sep     rd                  ; wait half the pulse width
           ghi     rf                  ; recover baud constant
           phi     re
recvlp:    ghi     rf
           shr                         ; shift right
           SERN    recvlp0             ; jump if zero bi
           ori     128                 ; set bit
recvlp1:   phi     rf
           sep     rd                  ; perform bit delay
           dec     rf                  ; decrement bit count
           nop
           nop
           glo     rf                  ; check for zero
           bnz     recvlp              ; loop if not
recvdone:  SERREQ
           sep     rd                  ; get past stop bit
           sep     rd
           ghi     rf                  ; get character
           plo     re
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rf
           ldx
           plo     rf
;;[RLA]    glo     re
;;[RLA]    shr
;;[RLA]    plo     re                  ; save char
           ghi     re                  ; get echo flag
           shr                         ; see if need echo
           glo     re                  ; get character
           bnf     noecho              ; jump if no echo needed
           stxd                        ; save on stack
           sep     scall               ; type the character out
           dw      f_type
           irx                         ; recover character
           ldx
noecho:    sep     sret                ; and return to caller
recvlp0:   br      recvlp1             ; equalize between 0 and 1

#else
; [RLA]   This version of read: is full duplex - it echos each bit as that
; [RLA] bit is received...
read:      push    rf                  ; save consumed registers
           push    rd
           ldi     9                   ; 8 bits to receive
           plo     rf
           mov     rd,delay            ; address of bit delay routine
           ghi     re                  ; first delay is half bit size
           phi     rf
           shr
           shr
           phi     re
           SERP    $                   ; wait for transmission
           sep     rd                  ; wait half the pulse width
           ghi     rf                  ; recover baud constant
           phi     re
           ghi     rf
           shr
           bdf     recvlpe
recvlp:    ghi     rf
           shr                         ; shift right
           SERN    recvlp0             ; jump if zero bi
           ori     128                 ; set bit
recvlp1:   phi     rf
           sep     rd                  ; perform bit delay
           dec     rf                  ; decrement bit count
           nop
           nop
           glo     rf                  ; check for zero
           bnz     recvlp              ; loop if not
recvdone:  SERREQ
           ghi     rf                  ; get character
           plo     re
           pop     rd                  ; recover consumed registers
           pop     rf
           glo     re
           sep     sret                ; and return to caller
recvlp0:   br      recvlp1             ; equalize between 0 and 1

recvlpe:   ghi     rf
           shr                         ; shift right
           SERN    recvlpe0            ; jump if zero bi
           ori     128                 ; set bit
           SERREQ
recvlpe1:  phi     rf
           sep     rd                  ; perform bit delay
           dec     rf                  ; decrement bit count
           sex     r2
           sex     r2
           glo     rf                  ; check for zero
           bnz     recvlpe             ; loop if not
           br      recvdone
recvlpe0:  SERSEQ
           br      recvlpe1
#endif


brktest:   adi     0                   ; clear DF flag
           SERP    nobreak
           SERN    $
           smi     0                   ; signal break condition on serial
nobreak:   sep     sret                ; and return

; *****************************************************
; *** Serial output with 0C translation to <ESC>[2J ***
; *****************************************************
tty:       plo     re                  ; save character
           smi     0ch                 ; compare against formfeed
           bz      ttyff               ; jump if formfeed
           glo     re                  ; recover byte
ttyend:    lbr     f_tty               ; and display character
ttyff:     sep     scall               ; display vt100 sequence to clear screen
           dw      typeinmsg
           db      01bh,'[2J',0
           sep     sret                ; and return to caller


           org     BASE+0d00h
initcall:  ldi     high ret
           dec     r2
           dec     r2
           phi     r5
           ldi     low ret
           plo     r5
           ldi     high call
           phi     r4
           ldi     low call
           plo     r4
           sep     r5

; **** Strcmp compares the strings pointing to by R(D) and R(F)
; **** Returns:
; ****    R(F) = R(D)     0
; ****    R(F) < R(D)     -1 (255)
; ****    R(F) > R(D)     1
strcmp:  lda     rd          ; get next byte in string
         ani     0ffh        ; check for zero
         bz      strcmpe     ; found end of first string
         stxd                ; store into memory
         irx
         lda     rf          ; get byte from first string
         sm                  ; subtract 2nd byte from it
         bz      strcmp      ; so far a match, keep looking
         bnf     strcmp1     ; jump if first string is smaller
         ldi     1           ; indicate first string is larger
         lskp                ; and return to caller
strcmp1: ldi     255         ; return -1, first string is smaller
         sep     sret        ; return to calelr
strcmpe: lda     rf          ; get byte from second string
         bz      strcmpm     ; jump if also zero
         ldi     1           ; first string is smaller (returns -1)
         sep     sret        ; return to caller
strcmpm: ldi     0           ; strings are a match
         sep     sret        ; return to caller

; **** ltrim trims leading white space from string pointed to by R[F]
; **** Returns:
; ****    R(F) pointing to non-whitespace portion of string
ltrim:   ldn     rf          ; get next byte from string
         lbz     return      ; return if at end of string
         smi     ' '+1       ; looking for anthing <= space
         lbdf    return      ; found first non white-space
         inc     rf          ; point to next character
         br      ltrim       ; keep looking

; **** strcpy copies string pointed to by R[F] to R[D]
strcpy:  lda    rf           ; get byte from source string
         str    rd           ; store into destination
         lbz    return       ; return if copied terminator
         inc    rd           ; increment destination pointer
         br     strcpy       ; continue looping

; **** memcpy copies R[C] bytes from R[F] to R[D]
memcpy:  glo    rc           ; get low count byte
         bnz    memcpy1      ; jump if not zero
         ghi    rc           ; get high count byte
         lbz    return       ; return if zero
memcpy1: lda    rf           ; get byte from source
         str    rd           ; store into destination
         inc    rd           ; point to next destination position
         dec    rc           ; decrement count
         br     memcpy       ; and continue copy

; *** RC:RB = RF * RD (RB is low word)
; *** R(X) must point to suitable stack
mul16:     ldi     0                   ; zero out total
           phi     rb
           plo     rb
           phi     rc
           plo     rc
           sex     r2                  ; make sure X points to stack
mulloop:   glo     rd                  ; get low of multiplier
           bnz     mulcont             ; continue multiplying if nonzero
           ghi     rd                  ; check hi byte as well
           bnz     mulcont
           sep     sret                ; return to caller
mulcont:   ghi     rd                  ; shift multiplier
           shr     
           phi     rd
           glo     rd
           shrc    
           plo     rd
           bnf     mulcont2            ; loop if no addition needed
           glo     rf                  ; add F to C:B
           str     r2
           glo     rb
           add     
           plo     rb
           ghi     rf
           str     r2
           ghi     rb
           adc
           phi     rb
           glo     rc                  ; carry into high word
           adci    0
           plo     rc
           ghi     rc
           adci    0
           phi     rc
mulcont2:  glo     rf                  ; shift first number
           shl
           plo     rf
           ghi     rf
           shlc
           phi     rf
           br      mulloop             ; loop until done

; *** RB = RF/RD
; *** RF = Remainder
; *** uses R8 and R9
div16:     ldi     0                   ; clear answer
           phi     rb
           plo     rb
           phi     r8                  ; set additive
           plo     r8
           inc     r8
           glo     rd                  ; check for divide by 0
           bnz     d16lp1
           ghi     rd
           bnz     d16lp1
           ldi     0ffh                ; return 0ffffh as div/0 error
           phi     rb
           plo     rb
           sep     sret     
d16lp1:    ghi     rd                  ; get high byte from r7
           ani     128                 ; check high bit
           bnz     divst               ; jump if set
           glo     rd                  ; lo byte of divisor
           shl                         ; multiply by 2
           plo     rd                  ; and put back
           ghi     rd                  ; get high byte of divisor
           shlc                        ; continue multiply by 2
           phi     rd                  ; and put back
           glo     r8                  ; multiply additive by 2
           shl
           plo     r8
           ghi     r8
           shlc
           phi     r8
           br      d16lp1              ; loop until high bit set in divisor
divst:     glo     r8                  ; get low of divisor
           bnz     divgo               ; jump if still nonzero
           ghi     r8                  ; check hi byte too
           lbz     return              ; jump if done
divgo:     ghi     rf                  ; copy dividend
           phi     r9
           glo     rf
           plo     r9
           glo     rd                  ; get lo of divisor
           stxd                        ; place into memory
           irx                         ; point to memory
           glo     rf                  ; get low byte of dividend
           sm                          ; subtract
           plo     rf                  ; put back into r6
           ghi     rd                  ; get hi of divisor
           stxd                        ; place into memory
           irx                         ; point to byte
           ghi     rf                  ; get hi of dividend
           smb                         ; subtract
           phi     rf                  ; and put back
           bdf     divyes              ; branch if no borrow happened
           ghi     r9                  ; recover copy
           phi     rf                  ; put back into dividend
           glo     r9
           plo     rf
           br      divno               ; jump to next iteration
divyes:    glo     r8                  ; get lo of additive
           stxd                        ; place in memory
           irx                         ; point to byte
           glo     rb                  ; get lo of answer
           add                         ; and add
           plo     rb                  ; put back
           ghi     r8                  ; get hi of additive
           stxd                        ; place into memory
           irx                         ; point to byte
           ghi     rb                  ; get hi byte of answer
           adc                         ; and continue addition
           phi     rb                  ; put back
divno:     ghi     rd                  ; get hi of divisor
           shr                         ; divide by 2
           phi     rd                  ; put back
           glo     rd                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     rd
           ghi     r8                  ; get hi of divisor
           shr                         ; divide by 2
           phi     r8                  ; put back
           glo     r8                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     r8
           br      divst               ; next iteration

ttyold:    lbr     tty

; *****************************************
; *** See if D is alphanumeric          ***
; *** Returns: DF=0 - not valid         ***
; ***          DF=1 - is valid          ***
; *****************************************
isalnum:   plo     re                  ; keep copy of D
           sep     scall               ; check if numeric
           dw      isnum
           lbdf    passes              ; jump if numeric
           sep     scall               ; check for alpha
           dw      isalpha
           lbdf    passes              ; jump if alpha
           lbr     fails               ; otherwise fails

; ***********************************
; *** Check for symbol terminator ***
; *** Returns: DF=1 - terminator  ***
; ***********************************
isterm:    sep     scall               ; see if alphanumeric
           dw      isalnum
           lbdf    fails               ; fails if so
           lbr     passes

;;[RLA]   The BIOS assembles OK even if this next part isn't aligned, and we
;;[RLA] we desperately need those extra couple of bytes at $FF00!
#ifndef ELF2K
           org     BASE+0e00h
#endif
; ******************************************
; *** Check if symbol is in symbol table ***
; *** RF - pointer to ascii symbol       ***
; *** R7 - pointer to token table        ***
; *** Returns: RD - function number      ***
; ***          DF=1 - is function        ***
; ***          DF=0 - is not a function  ***
; ******************************************
tokenfunc: glo     r7                  ; save position of R7
           stxd
           ghi     r7
           stxd
           glo     rb                  ; save other consumed register
           stxd
           ghi     rb
           stxd
           ldi     0                   ; setup function number
           plo     rd
           ghi     rf                  ; save buffer position
           phi     rb
           glo     rf
           plo     rb
tfloop:    ldn     r7                  ; see if at last token
           bnz     tfgo                ; jump if not
           adi     0                   ; signal symbol not found
tfreturn:  irx                         ; recover consumed registers
           ldxa
           phi     rb
           ldxa
           plo     rb
           ldxa
           phi     r7
           ldx
           plo     r7
           sep     sret                ; and return
tfgo:      ldn     r7                  ; get token byte
           ani     080h                ; see if last one
           bnz     tflast              ; jump if it was
           lda     r7                  ; get byte from function table
           str     r2                  ; setup compare
           lda     rf                  ; get byte from input
           sm                          ; and compare
           bz      tfgo                ; loop back if match
tfnolp:    lda     r7                  ; need to find end of token
           ani     080h
           bz      tfnolp              ; loop until found
tfno:      inc     rd                  ; increment function number
           ghi     rb                  ; restore buffer position
           phi     rf
           glo     rb
           plo     rf
           br      tfloop              ; loop to check next token
tflast:    lda     r7                  ; get byte from token
           ani     07fh                ; strip high bit
           str     r2                  ; store for compare
           lda     rf                  ; get byte from buffer
           sm                          ; and see if a match
           bnz     tfno                ; jump if not
           smi     0                   ; signal match found
           br      tfreturn            ; and return

; ***********************************************
; *** identify symbol as decimal, hex, or non ***
; *** RF - pointer to symbol                  ***
; *** Returns: D=0 - decimal number           ***
; ***          D=1 - hex number               ***
; ***          DF=1 - non numeric             ***
; ***          DF=0 - is numeric              ***
; ***********************************************
idnum:     glo     rf                  ; save position
           stxd
           ghi     rf
           stxd
           ldn     rf                  ; get first byte
           sep     scall               ; must be numeric
           dw      isnum
           bdf     idlp1               ; jump if it was
idnumno:   smi     0                   ; signal non-numeric
           lskp
idnumyes:  adi     0                   ; signal numeric
           plo     re                  ; save number
           irx                         ; recover RF
           ldxa
           phi     rf
           ldx
           plo     rf
           glo     re                  ; recover number
return:    sep     sret                ; and return to caller
idlp1:     lda     rf                  ; get next byte
           sep     scall               ; check for symbol terminator
           dw      isterm
           bdf     iddec               ; signal decimal number
           sep     scall               ; see if char is numeric
           dw      isnum
           bdf     idlp1               ; jump if so
           dec     rf                  ; move back to char
idlp2:     lda     rf                  ; get next byte
           sep     scall               ; see if terminator
           dw      isterm
           bdf     idnumno             ; jump if term found before h
           sep     scall               ; check for hex character
           dw      ishex
           bdf     idlp2               ; loop back if so
           smi     'H'                 ; check for final H
           bz      idhex               ; jump if hex
           smi     32                  ; check for h
           bz      idhex
           br      idnumno             ; was not proper number
iddec:     ldi     0                   ; signal decimal number
           br      idnumyes            ; and return
idhex:     ldi     1                   ; signal hex number
           br      idnumyes            ; and return



; **** Find last available memory address
; **** Returns: RF - last writable address
#ifdef MCHIP
freemem:   ldi     080h      ; start from beginning of memory
#else
freemem:   ldi     000h      ; start from beginning of memory
#endif
           phi     rf        ; place into register
           ldi     0ffh
           plo     rf
           sex     r2        ; be sure x points to stack
fmemlp:    ldn     rf        ; get byte
           plo     re        ; save a copy
           xri     255       ; flip the bits
           str     rf        ; place into memory
           ldn     rf        ; retrieve from memory
           stxd              ; place into memory
           irx               ; point to previous value
           glo     re
           sm                ; and compare
           bz      fmemdn    ; jump if not different
           glo     re        ; recover byte
           str     rf        ; write back into memory
           ghi     rf
           adi     1
           phi     rf
;;[RLA]    bnz     fmemlp    ; jump if not
#ifdef STGROM
;[RLA]   For the Elf 2000, we never want to use page 07FxxH - that belongs to
;[RLA] the Elf 2000 monitor program.  Likewise, if the 80 column video card
;[RLA] is installed then we need to reserve another 2K off the top of SRAM
;[RLA] for the frame buffer.  Sadly, theres no easy way for the BIOS to know
;[RLA] if the video card is installed, but we can make a pretty good guess
;[RLA] by checking RE.1 (the baud rate constant) - if this is zero, then the
;[RLA] video card is being used as the console!
#ifdef VIDEO
           ghi     re        ; [RLA] get the baud rate constant
           ani     0feh      ; [RLA] mask out local echo bit
           xri     0feh      ; [RLA] is the 80 column video card in use?
           bnz     fmeml1    ; [RLA] branch if no video card
           ghi     rf        ; [RLA] get the current page
           smi     077h      ; [RLA] and stop at page 77H
           br      fmeml2    ; [RLA] ...
#endif
;[RLA] No video card - test memory up to 07EFFH ...
fmeml1:    ghi     rf        ; [RLA] get the current page
           smi     07fh      ; [RLA] and stop at page 7FH
fmeml2:
#else
           ghi     rf
#endif
           bnz     fmemlp
fmemdn:    ghi     rf        ; point back to last writable memory
           smi     1
           phi     rf
           sep     sret      ; and return to caller

;[RLA]   This is a "cold" boot - it will set up SCRT and a temporary stack
;[RLA] before calling the "warm" boot at bootret: ...
bootide:   ldi     high bootret        ; prepare for seetin call
           phi     r6
           ldi     low bootret
           plo     r6
           ldi     0                   ; setup temp stack
           phi     r2
           ldi     0f0h
           plo     r2
           sex     r2
           lbr     f_initcall

;[RLA]   This routine will attempt to boot ElfOS from the primary (i.e. master)
;[RLA] IDE drive.  It returns with DF=1 if there's a hardware error (e.g. no
;[RLA] drive, read error, etc) and with DF=0 if the hardware is OK but the drive
;[RLA] does not contain a bootable ElfOS system.  Of course, if it works then
;[RLA] this routine never returns ...
bootret:   ldi     0                   ; select master drive
           plo     rd
           sep     scall               ; reset ide drive
           dw      f_iderst
#ifdef EIDE
           lbdf    err                 ; [RLA] quit now if error!
#endif
           ldi     0                   ; prepare to read sector 0
           plo     r7
           phi     r7
           plo     r8
           plo     rf
           ldi     0e0h
           phi     r8
           ldi     1
           phi     rf
           sep     scall
           dw      f_ideread
#ifdef EIDE
           lbdf    err                 ; return to monitor if disk error
           sep     scall               ; check boot code
           dw      btcheck
           lbdf    err                 ; error out on mismatch
#endif
           lbr     0106h

input256:  ldi     1                   ; allow 256 input bytes
           phi     rc
           ldi     0
           plo     rc
           lbr     input

; ***************************************
; *** Type message pointed to by R[F] ***
; ***************************************
typemsg:   lda     rf                  ; load byte from message
           lbz     return              ; return if last byte
           sep     scall               ; call type routine
           dw      f_tty
           br      typemsg             ; loop until a zero found

; ******************************************
; *** Get supported devices list         ***
; *** Returns: RF - device list          ***
; ***          Bit 0 - IDE               ***
; ***          Bit 1 - Floppy            ***
; ***          Bit 2 - Bit-banged serial ***
; ***          Bit 3 - UART              ***
; ***          Bit 4 - RTC               ***
; ***          Bit 5 - NVR               ***
; ******************************************

DEVICES:equ     05h     ; all versions support IDE and bit banged serial
#ifdef UART
UARTDEV:equ     08h     ; UART is supported too
#else
UARTDEV:equ     0       ; no UART
#endif
#ifdef NVR
NVRDEV: equ     30h     ; RTC and NVR supported
#else
NVRDEV: equ     0       ; no RTC nor NVR
#endif

getdev:    ldi     DEVICES+UARTDEV+NVRDEV  ; load map of supported devices
           plo     rf                  ; ...
           ldi     0                   ; high byte is zero
           phi     rf                  ; ...
           sep     sret                ; return
; #endif

numbers:   db      027h,010h,3,0e8h,0,100,0,10,0,1

           org     BASE+0f00h
f_boot:    lbr     bootide
#ifdef UART
f_type:    lbr     e2k_tx
#else
f_type:    lbr     type
#endif
#ifdef UART
f_read:    lbr     e2k_rx
#else
f_read:    lbr     read
#endif
f_msg:     lbr     typemsg
f_typex:   lbr     return
f_input:   lbr     input256
f_strcmp:  lbr     strcmp
f_ltrim:   lbr     ltrim
f_strcpy:  lbr     strcpy
f_memcpy:  lbr     memcpy
f_wrtsec:  lbr     0
f_rdsec:   lbr     0
f_seek0:   lbr     0
f_seek:    lbr     0
f_drive:   lbr     0
f_setbd:   lbr     timalc
f_mul16:   lbr     mul16
f_div16:   lbr     div16
f_iderst:  lbr     resetide
f_idewrt:  lbr     wrtide
f_ideread: lbr     rdide
f_initcall: lbr    initcall
f_ideboot: lbr     bootret
f_hexin:   lbr     hexin
f_hexout2: lbr     hexout2
f_hexout4: lbr     hexout4
#ifdef UART
f_tty:     lbr     e2k_tx
#else
f_tty:     lbr     type
#endif
f_mover:   lbr     return
f_minimon: lbr     minimon
f_freemem: lbr     freemem
f_isnum:   lbr     isnum
f_atoi:    lbr     atoi
f_uintout: lbr     uintout
f_intout:  lbr     intout
f_inmsg:   lbr     typeinmsg
f_inputl:  lbr     input
#ifdef UART
f_brktest: lbr     e2k_brk
#else
f_brktest: lbr     brktest
#endif
f_findtkn: lbr     tokenfunc
f_isalpha: lbr     isalpha
f_ishex:   lbr     ishex
f_isalnum: lbr     isalnum
f_idnum:   lbr     idnum
f_isterm:  lbr     isterm
f_getdev:  lbr     getdev
f_nbread:  lbr     read
f_sdread:  lbr     sdread
f_sdwrite: lbr     sdwrite
f_sdreset: lbr     resetide

input:     glo     ra                  ; save RA
           stxd
           ghi     ra
           stxd
           ldi     0                   ; byte count
           plo     ra                  ; store into counter
inplp:     sep     scall               ; call input function
           dw      f_read
           plo     re                  ; save char
           smi     3                   ; check for <CTRL><C>
           bz      inpterm             ; terminate input
           smi     5                   ; check for <BS>
           bz      isbs                ; jump if so
           smi     5                   ; check for <CR>
           bz      inpdone             ; jump if so
           glo     rc                  ; check count
           bnz     inpcnt              ; jump if can continue
           ghi     rc                  ; check high of count
           bnz     inpcnt
           ldi     8                   ; performa backspace
           sep     scall
           dw      f_tty
           br      bs2                 ; remove char from screen
inpcnt:    glo     re
           str     rf                  ; store into output
           inc     rf                  ; point to next position
           smi     08                  ; look for backspace
           bnz     nobs                ; jump if not a backspace
isbs:      glo     ra                  ; get input count
           bz      inplp               ; disregard if string is empty
           dec     ra                  ; decrement the count
           dec     rf                  ; decrement buffer position
           inc     rc                  ; increment allowed characters

bs2:       sep     scall
           dw      f_inmsg
           db      32,8,0

           br      inplp               ; and loop back for more
nobs:      inc     ra                  ; increment input count
           dec     rc                  ; decrement character count
           br      inplp               ; and then loop back
inpdone:   ldi     0                   ; need a zero terminator
           shr                         ; reset DF flag, to show valid input
inpdone2:  str     rf                  ; store into buffer
           irx                         ; recover RA
           ldxa
           phi     ra
           ldx
           plo     ra
           sep     sret                ; return to caller
inpterm:   smi     0                   ; signal <CTRL><C> exit
           ldi     0                   ; finish
           br      inpdone2
          

; *****************************************************
; *** Function to implement a stack based call      ***
; ***    RX is assumed to be the stack pointer      ***
; ***    R3 is assumed to be the main PC            ***
; ***    R6 will hold return address                ***
; ***    usage is:    sep R4                        ***
; ***                 dw  call_addr                 ***
; *****************************************************
         org     BASE+0fe0h
         lbr     call
         org     BASE+0ff1h
         lbr     ret

         org     BASE+0ff9h
version: db      1,0,13
chsum:   db      0,0,0,0
