data:   equ     0

        org     0ff00h
return: sep     r3
bios:   lbz     boot         ; jump if cold boot (00)
        smi     01h          ; check for type  (01)
        lbz     type         ; jump to type6 routine
        smi     01h          ; check for get char (02)
        lbz     read
        smi     01h          ; check for type message (03)
        lbz     typemsg
        smi     01h          ; check for typex (04)
        lbz     typex        ; jump to typex routine
        smi     01h          ; check for input (05)
        lbz     input        ; jump to input routine
        smi     01h          ; check for string compare (06)
        lbz     strcmp       ; jump to string compare routine
        smi     01h          ; check for ltrim function (07)
        lbz     ltrim        ; jump if found
        smi     01h          ; check for strcpy function (08)
        lbz     strcpy       ; jump if found
        smi     01h          ; check for memcpy function (09)
        lbz     memcpy       ; jump if found
        smi     01h          ; check for write sector function (10)
        lbz     wrtsec       ; jump if found
        smi     01h          ; check for read sector function (11)
        lbz     rdsec        ; jump if found
        smi     01h          ; check for track 0 function  (12)
        lbz     trk0         ; jump if found
        smi     01h          ; check for track seek function  (13)
        lbz     seek         ; jump if found
        smi     01h          ; check for select drive function  (14)
        lbz     drvsel       ; jump if found
        smi     01h          ; check for get terminal setting (15)
        lbz     timalc       ; jump if found
        smi     01h          ; check for get mul 8 (16)
        lbz     mul8         ; jump if found
        smi     01h          ; check for get dev 16 (17)
        lbz     div16        ; jump if found
        smi     01h          ; check for ide reset (18)
        lbz     resetide     ; jump if found
        smi     01h          ; check for ide write (19)
        lbz     wrtide       ; jump if found
        smi     01h          ; check for ide read (20)
        lbz     rdide        ; jump if found

        br     return        ; return to caller

returna: ldi   high ret4
         phi   rb
         ldi   low ret4
         plo   rb
         sep   rb
ret4:    glo   ra
         plo   r3
         ghi   ra
         phi   r3
         br   return

typemsg: glo   r3            ; get callers address
         plo   ra            ; save for later
         ghi   r3            ; high part
         phi   ra
         ldi   low typelp    ; get type loop address
         plo   r3            ; and place into r5
         ldi   high typelp
         phi   r3
         sep   r3
typelp:  ldx                 ; load byte from message
         lbz   returna       ; return if last byte
         ldi   high bios     ; get address of call routine
         phi   rb            ; place into register 3
         ldi   low  bios     ; get low portion of address
         plo   rb
         ldi   4             ; function to use type4
         sep   rb            ; perform the function
         br    typelp        ; loop until a zero found

input:   glo   r3            ; get callers address
         plo   ra            ; save for later
         ghi   r3            ; high part
         phi   ra
         ldi   low inplp     ; get type loop address
         plo   r3            ; and place into r5
         ldi   high inplp
         phi   r3
         ldi   0             ; byte count
         plo   rc            ; store into counter
         sep   r3
inplp:   ldi   high bios     ; get address of call routine
         phi   rb            ; place into register 3
         ldi   low  bios     ; get low portion of address
         plo   rb
         ldi   2             ; function to read key
         mark                ; save x
         sep   rb            ; perform the function
         sex   r2            ; point x at stack
         irx                 ; point to old x
         ret                 ; recover x
         dec   r2            ; restore stack pointer
         stxd                ; store byte
         irx                 ; point to next position
         irx                 ; point to next position
         smi   08            ; look for backspace
         bnz   nobs          ; jump if not a backspace
         glo   rc            ; get input count
         bz    inplp         ; disregard if string is empty
         dec   rc            ; decrement the count
         stxd                ; decrement buffer position
         stxd    
         br    inplp         ; and loop back for more
nobs:    smi   05            ; check for CR
         bz    inpdone       ; loop back if not
         inc   rc            ; increment input count
         br    inplp         ; and then loop back
inpdone: ldi   0             ; need a zero terminator
         stxd                ; store into buffer
         lbr   returna       ; return to caller


; *****************************************************
; *** Function to implement a stack based call      ***
; ***    R2 is assumed to be the stack pointer      ***
; ***    R3 is assumed to be the main PC            ***
; ***    RF is consumed                             ***
; ***    usage is:    sep R4                        ***
; ***                 dw  call_addr                 ***
; *** Routine saves R3 values onto the stack        ***
; *** and and sets it to the call address           ***
; *****************************************************
         org     0ffdfh
         sep     r3                    ; jump to called routine
call:    phi     rf                    ; save D
         sex     r2                    ; set x to stack segment
         lda     r3                    ; get high byte and advance to low
         plo     rf                    ; save it
         inc     r3                    ; move past low address
         glo     r3                    ; get low value of return address
         stxd                          ; store onto stack
         ghi     r3                    ; get high value of return address
         stxd                          ; and place onto stack
         dec     r3                    ; point to low byte
         ldn     r3                    ; get low byte
         plo     r3                    ; and place into low byte of PC
         glo     rf                    ; recover high byte
         phi     r3                    ; put into high of PC
         ghi     rf                    ; recover D
         br      call-1                ; transfer control to subroutine

         sep     r3                    ; transfer control back to coller
ret:     phi     rf                    ; save return value
         inc     r2                    ; point to high byte of return address
         lda     r2                    ; get high byte
         phi     r3                    ; put into register 5
         ldn     r2                    ; get low byte
         plo     r3                    ; put into low
         ghi     rf                    ; recall return value
         br      ret-1                 ; and perform return to caller

         org     0fe00h

; **** Write sector to disk, R(6) points to data
; ****    R(6) must point to an even 256 byte boundary
; ****    RC.0 = sector
; ****  Returns: D - write status
wrtsec:  ldi     low data    ; get address of scratchpad
         adi     4           ; point to end of command
         plo     rf          ; register to use to write temp data
         ldi     high data   ; get high address of scratchpad
         phi     rf          ; write to register
         sex     rf          ; point data register to command buffer
         ldi     3           ; data register address
         stxd                ; write to command
         ldi     0a4h        ; command to initiate writing
         stxd                ; write to command
         ldi     0           ; command register address
         stxd                ; write to command
         glo     rc          ; get sector
         stxd                ; write to command
         ldi     2           ; sector register address
         str     rf          ; write to command port
         out     2           ; write sector register to selector
         out     3           ; send sector
         out     2           ; write command register to selector
         out     3           ; send write command
         out     2           ; select data port
         sex     r6          ; point data register to data
wrtlp:   b2      $
         out     3
         glo     r6
         bnz     wrtlp

;wrtgo:   ldi     255         ; set timeout timer
;wrtlp:   bn2     wrtrdy      ; jump if byte is ready to write
;         smi     1           ; subtract 1 from timeout
;         bnz     wrtlp       ; loop back if still more time
;         br      dskstat     ; get disk status
;wrtrdy:  out     3           ; write byte to controller
;         glo     r6          ; get number of bytes
;         bnz     wrtgo       ; jump if more bytes to write
dskstat: ldi     0           ; status port
         str     rf          ; write to scratchpad
         sex     rf          ; point data register to scratchpad
         out     2           ; select status port
         dec     rf          ; point to scratch area
statlp:  inp     3           ; read status
         shr                 ; shift busy bit into DF
         bdf     statlp      ; loop until no longer busy
         shl                 ; shift back into position
         lbr     return      ; return status code

; **** Read sector from disk, R(6) points to buffer
; ****    R(6) must point to an even 256 byte boundary
; ****    RC.0 = sector
; ****  Returns: D - read status
rdsec:   ldi     low data    ; get address of scratchpad
         adi     4           ; point to end of command
         plo     rf          ; register to use to write temp data
         ldi     high data   ; get high address of scratchpad
         phi     rf          ; write to register
         sex     rf          ; point data register to command buffer
         ldi     3           ; data register address
         stxd                ; write to command
         ldi     084h        ; command to initiate reading
         stxd                ; write to command
         ldi     0           ; command register address
         stxd                ; write to command
         glo     rc          ; get sector
         stxd                ; write to command
         ldi     2           ; sector register address
         str     rf          ; write to command port
         out     2           ; write sector register to selector
         out     3           ; send sector
         out     2           ; write command register to selector
         out     3           ; send write command
         out     2           ; select data port
         sex     r6          ; point data register to data
rdlp:    b2      $
         inp     3
         irx
         glo     r6
         bnz     rdlp
;rdgo:    ldi     255         ; set timeout value
;rdlp:    bn2     rdeady      ; jump if byte ready to read
;         smi     1           ; subtract 1 from timeout
;         bnz     rdlp        ; check again if time left
;         br      dskstat     ; error occured, return status
;rdeady:  inp     3           ; read byte from controller
;         irx                 ; increment pointer
;         glo     r6          ; get pointer
;         bnz     rdgo        ; loop until 256 bytes read
         br      dskstat     ; jump to get status

; **** Select Drive
; ****   RC.0 = drive (1=drive 1,2=drive 2,4=drive 3,8=drive 4)
drvsel:  ldi     low data    ; get address of scratchpad
         adi     4           ; point to end of command
         plo     rf          ; register to use to write temp data
         ldi     high data   ; get high address of scratchpad
         phi     rf          ; write to register
         sex     rf          ; point data register to command buffer
         glo     rc          ; get requested drive
         stxd                ; store into command buffer
         ldi     4           ; drive select register
         str     rf          ; store into command buffer
         out     2           ; write drive select to selector
         out     3           ; write drive select register
         lbr     return      ; return to caller

; **** Restore to track 0
trk0:    ldi     low data    ; get address of scratchpad
         adi     4           ; point to end of command
         plo     rf          ; register to use to write temp data
         ldi     high data   ; get high address of scratchpad
         phi     rf          ; write to register
         sex     rf          ; point data register to command buffer
         ldi     09          ; command to do a disk restore
         stxd                ; write to command buffer
         ldi     0           ; command port
         str     rf          ; write to command port
         out     2           ; write command register to selector
         out     3           ; issue restore command
         br      dskstat     ; branch to get diskstat

; **** Seek to track
; ****    RC.0 = track
; ****  Returns: D - read status
seek:    ldi     low data    ; get address of scratchpad
         adi     4           ; point to end of command
         plo     rf          ; register to use to write temp data
         ldi     high data   ; get high address of scratchpad
         phi     rf          ; write to register
         sex     rf          ; point data register to command buffer
         ldi     19h         ; command to do a disk seek
         stxd                ; write to command buffer
         ldi     0           ; command port
         stxd                ; write to command buffer
         glo     rc          ; get passed track
         stxd                ; write to command buffer
         ldi     3           ; data register selector
         str     rf          ; write to command port
         out     2           ; write data port to selector
         out     3           ; write track to data register
         out     2           ; write command port to selector
         out     3           ; output the command
         br      dskstat     ; branch to get command status

; **** Boot drive 0, track 0, sector 0 ****
boot:    ldi     low boot1   ; get address of boot routine
         plo     r5          ; place into r5
         ldi     high boot1  ; get high part
         phi     r5          ; and place into register
         sep     r5          ; transfer to new program counter
boot1:   ldi     low call    ; get address of bios table
         plo     r3          ; place into r3
         ldi     high call   ; high portion
         phi     r3          ; place into register
         ldi     12          ; function to seek to track 0
         sep     r3          ; perform seek
         ldi     01          ; high portion of disk buffer address
         phi     r6          ; place into r6
         plo     r0          ; place into start register as well
         ldi     00          ; address of disk buffer
         plo     r6          ; place into r6
         plo     r0          ; place into start register as well
         plo     rc          ; also place 0 into sector number
         ldi     11          ; bios function to read disk sector
         sep     r3          ; read the sector
         sep     r0          ; transfer control to read sector

         org     0fd00h
; **** Strcmp compares the strings pointing to by R(6) and R(X)
; **** Returns:
; ****    R(6) = R(X)     0
; ****    R(6) < R(X)     -1 (255)
; ****    R(6) > R(X)     1
strcmp:  lda     r6          ; get next byte in string
         ani     0ffh        ; check for zero
         bz      strcmpe     ; found end of first string
         sm                  ; subtract 2nd byte from it
         irx                 ; point to next character
         bz      strcmp      ; so far a match, keep looking
         bnf     strcmp1     ; jump if first string is smaller
         ldi     1           ; indicate first string is larger
         lskp                ; and return to caller
strcmp1: ldi     255         ; return -1, first string is smaller
         lbr     return      ; return to calelr
strcmpe: ldx                 ; get byte from second string
         bz      strcmpm     ; jump if also zero
         ldi     255         ; first string is smaller (returns -1)
         lbr     return      ; return to caller
strcmpm: ldi     0           ; strings are a match
         lbr     return      ; return to caller

; **** ltrim trims leading white space from string pointed to by R[X]
; **** Returns:
; ****    R(X) pointing to non-whitespace portion of string
ltrim:   ldx                 ; get next byte from string
         lbz     return      ; return if at end of string
         smi     ' '+1       ; looking for anthing <= space
         lbdf    return      ; found first non white-space
         irx                 ; point to next character
         br      ltrim       ; keep looking

; **** strcpy copies string pointed to by R[X] to R[6]
strcpy:  ldxa                ; get byte from source string
         str    r6           ; store into destination
         lbz    return       ; return if copied terminator
         inc    r6           ; increment destination pointer
         br     strcpy       ; continue looping

; **** memcpy copies R[F] bytes from R[X] to R[6]
memcpy:  glo    rf           ; get low count byte
         bnz    memcpy1      ; jump if not zero
         ghi    rf           ; get high count byte
         lbz    return       ; return if zero
memcpy1: ldxa                ; get byte from source
         str    r6           ; store into destination
         inc    r6           ; point to next destination position
         dec    rf           ; decrement count
         br     memcpy       ; and continue copy

; *** RF = a x b   
; *** R6.0 = a 
; *** R6.1 = b
; *** R(X) must point to suitable stack

mul8:      ghi     r6                  ; get b
           stxd                        ; store to stack
           ldi     0                   ; zero out total
           phi     rf
           plo     rf
           phi     r6                  ; clear high byte of a

mul8lp:    irx                         ; point to b
           ldx                         ; get b 
           lbz     return              ; jump if rest of b is zero
           shr                         ; shift lowest bit to DF
           stxd                        ; and put back
           bnf     noadd8              ; jump if bit was zero
           glo     r6                  ; get lo byte of a
           stxd                        ; place into memory
           irx                         ; point to byte
           glo     rf                  ; get lo byte of total
           add                         ; add together
           plo     rf                  ; put into lo byte of total
           ghi     r6                  ; get high byte of a
           stxd                        ; store into memory
           irx                         ; point to byte
           ghi     rf                  ; get high of total
           adc                         ; add together
           phi     rf                  ; put back into hi byte
noadd8:    glo     r6                  ; get lo byte of a
           shl                         ; multiply by 2
           plo     r6                  ; put back
           ghi     r6                  ; get high byte
           shlc                        ; propagate multiply by 2
           phi     r6                  ; put back
           br      mul8lp              ; loop until b is zero

; *** RF = R6/R7
; *** R6 = remainder
; *** uses R8 and R9
div16:     ldi     0                   ; clear answer
           phi     rf
           plo     rf
           phi     r8                  ; set additive
           plo     r8
           inc     r8
           glo     r7                  ; check for divide by 0
           bnz     d16lp1
           ghi     r7
           bnz     d16lp1
           ldi     0ffh                ; return 0ffffh as div/0 error
           phi     rf
           plo     rf
           lbr     return   
d16lp1:    ghi     r7                  ; get high byte from r7
           ani     128                 ; check high bit
           bnz     divst               ; jump if set
           glo     r7                  ; lo byte of divisor
           shl                         ; multiply by 2
           plo     r7                  ; and put back
           ghi     r7                  ; get high byte of divisor
           shlc                        ; continue multiply by 2
           phi     r7                  ; and put back
           glo     r8                  ; multiply additive by 2
           shl
           plo     r8
           ghi     r8
           shlc
           phi     r8
           br      d16lp1              ; loop until high bit set in divisor
divst:     glo     r7                  ; get low of divisor
           bnz     divgo               ; jump if still nonzero
           ghi     r7                  ; check hi byte too
           lbz     return              ; jump if done
divgo:     ghi     r6                  ; copy dividend
           phi     r9
           glo     r6
           plo     r9
           glo     r7                  ; get lo of divisor
           stxd                        ; place into memory
           irx                         ; point to memory
           glo     r6                  ; get low byte of dividend
           sm                          ; subtract
           plo     r6                  ; put back into r6
           ghi     r7                  ; get hi of divisor
           stxd                        ; place into memory
           irx                         ; point to byte
           ghi     r6                  ; get hi of dividend
           smb                         ; subtract
           phi     r6                  ; and put back
           bdf     divyes              ; branch if no borrow happened
           ghi     r9                  ; recover copy
           phi     r6                  ; put back into dividend
           glo     r9
           plo     r6
           br      divno               ; jump to next iteration
divyes:    glo     r8                  ; get lo of additive
           stxd                        ; place in memory
           irx                         ; point to byte
           glo     rf                  ; get lo of answer
           add                         ; and add
           plo     rf                  ; put back
           ghi     r8                  ; get hi of additive
           stxd                        ; place into memory
           irx                         ; point to byte
           ghi     rf                  ; get hi byte of answer
           adc                         ; and continue addition
           phi     rf                  ; put back
divno:     ghi     r7                  ; get hi of divisor
           shr                         ; divide by 2
           phi     r7                  ; put back
           glo     r7                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     r7
           ghi     r8                  ; get hi of divisor
           shr                         ; divide by 2
           phi     r8                  ; put back
           glo     r8                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     r8
           br      divst               ; next iteration

	org	0fc00h
; *** Software uart is adapted from that found in IDIOT/4
; *** I will probably have to change this in the future.
; Baud needs to have the high value set as follows
;      75 baud = 0a2h
;     110 baud = 06ch
;     300 baud = 026h
;     600 baud = 012h
;    1200 baud = 008h
; Add 1 for half duplex

delay:  equ     rc           ; register holding address for delay routine
char:   equ     rd
baud:   equ     re           ; register holding baud information
ascii:  equ     rf           ; register for ascii
typexit: lbr     return       ; return to caller
typex:  ldx                  ; get byte from m(x) and advance
        irx
        skp
type:   glo     ascii        ; get low byte of ascii
        plo     char         ; place byte into register D
        xri     0ah          ; check for linefeed
        bnz     ty2          ; jump if not
        ldi     5bh          ; more bits if need to wait
        lskp                 ; jump
ty2:    ldi     0bh          ; 11 bits to write
        plo     ascii
        ldi     delay1.1
        phi     delay
        ldi     delay1.0
        plo     delay
begin:  glo     baud         ; get baud delay flag
        lsz                  ; skip if no need to wait
        sep     delay        ; call delay routine
        db      23           ; 3 bit times
        seq                  ; begin start bit
nxtbit: sep     delay        ; call delay routine
        db      7            ; wait 1 bit time
        nop                  ; wait
        nop                  ; wait
        nop                  ; wait
        nop                  ; wait
        nop                  ; wait
        nop                  ; wait
        dec     ascii        ; decrement number of bits
        sdi     0
        glo     char         ; get next bit of character
        shrc                 ; lease significant bit first
        plo     char         ; put back
        lsdf                 ; long skip if bit =0
        seq                  ;   set q=1 - "space"
        lskp                 ; skip next 2 bytes
        req                  ; if bit = 1,
        nop                  ;    set q=0 = "mark"
        glo     ascii        ; until #bits = 0
        ani     0fh
        bnz     nxtbit       ; loop if more to go
        glo     ascii        ; get code byte
        adi     0fbh         ; decrement code
        plo     ascii        ; set #bits
        bnf     typexit      ; jump if no more
        smi     01bh         ; if code = 1
        bz      typexit      ; then was last null, exit
zer:    ldi     0            ; if code > 1
        plo     char         ; load byte
        br      begin        ; and type it

delext: sep     rb           ; return from delay routine
delay1: ghi     baud         ; get baud constant
        shr                  ; remove echo flag
        plo     baud         ; repeat
dellp2: dec     baud         ;   decrement baud
        lda     rb           ;   get #bits
dellp1: smi     1            ;   decrement until zero
        bnz     dellp1       ;
        glo     baud         ;   until baud = 0
        bz      delext       ; return if done
        dec     rb
        br      dellp2

rexit:  ghi     ascii        ; get character
        lbr     return       ; return to caller
read:  ldi     0            ; flag for terminal control
        plo     ascii        ; save entry flag
read2:  ldi     80h          ; set #bits in character = 7
        phi     ascii        ; (7 shift changes 80 into 01)
        ldi     delay1.1
        phi     delay
        ldi     delay1.0
        plo     delay
        sex     rb
        out     7            ; turn reader on
        db      80h
        bn4     $            ; wait while stop bit
tty1:   b4      $
        sep     delay        ; delay 1/2 bit time
        db      2
        b4      tty1
        out     7
        db      40h
nobit:  sex     r2           ; equalize delays
        sex     r2
bit:    ghi     baud
        shr
        bdf     noecho       ; check if need to echo
        b4      outbit
        seq                  ; set q if bit = 1
        lskp                 ; reset q if bit=0
outbit: req
noecho: nop                  ; equalize delays
        lsnf
        sex    r2
        sex    r2
        sex    r2
        nop
        nop
        sep    delay         ; wait 1 bit time
        db     7
        inc    baud          ; set delay flag = 1
        ghi    ascii         ; shift by 1 bit
        shr
        phi    ascii
        bdf    stop
        ori    80h
        bn4    nobit
        phi    ascii         ; continue loop
        br     bit
stop:   req                  ; set stop bit
        bz     read2         ; repeat if 00=null
        br     rexit         ; done
timalc:  ldi   high delay1
         phi   delay
         ldi   low delay1
         plo   delay
         ldi   0
         plo   baud
         plo   ascii
         b4    $
         bn4   $
         ldi   3
tc:      smi   1
         bnz   tc
         glo   ascii
         bnz   zto1
         b4    incr
         inc   ascii
zto1:    b4    daux
incr:    inc   baud
         ldi   7
         br    tc
daux:    dec   baud
         dec   baud
         glo   baud
         ori   1
         phi   baud
         sep   delay
         db    0ch
         bn4   wait
         ghi   baud
         ani   0feh
         phi   baud
wait:    sep   delay
         db    26h
         lbr   return

           org   0fb00h
resetide:  ldi     high waitrdy        ; get address of subs
           phi     rf                  ; place into rf
           ldi     low waitrdy
           plo     rf
           sex     r2                  ; point x to stack
           mark                        ; save current P and X
           sep     rf                  ; call wait for RDY routine
           dec     r2                  ; compensate for RET increment
           ldi     0                   ; code to select master
           stxd                        ; write to stack
           ldi     6                   ; select device register
           str     r2                  ; write to stack
           out     2                   ; write select port
           out     3                   ; write device code
           dec     r2                  ; point back
           ldi     2                   ; function to perform soft reset
           stxd                        ; write to stack
           ldi     00eh                ; set interupt port
           str     r2                  ; write to stack
           out     2                   ; write select port
           out     3                   ; write reset code
           dec     r2                  ; point back
           ldi     low waitrdy
           plo     rf
           mark                        ; save current P and X
           sep     rf                  ; call wait for RDY routine
           dec     r2                  ; compensate for RET increment
           ldi     0efh                ; command to set features
           stxd                        ; store on stack
           ldi     7                   ; command register address
           stxd                        ; store on stack
           ldi     1                   ; enable 8-bit mode
           stxd                        ; store on stack
           str     r2                  ; also is feature register address
           out     2                   ; selec feacture register
           out     3                   ; select 8-bit mode
           out     2                   ; select command register
           out     3                   ; issue set feature command
           dec     r2                  ; compensate for last out
           ldi     low waitrdy
           plo     rf
           mark                        ; save current P and X
           sep     rf                  ; call wait for RDY routine
           lbr     return              ; return to caller

           
wrtide:    ldi     high waitrdy        ; get address of subs
           phi     rf                  ; place into rf
           ldi     low waitrdy
           plo     rf
           sex     r2                  ; point x to stack
           mark                        ; save current P and X
           sep     rf                  ; call wait for RDY routine
           dec     r2                  ; compensate for RET increment
           ldi     030h                ; command for sector write
           mark                        ; save current P and X
           sep     rf                  ; now call command sequence
           dec     r2                  ; compensate for RET increment
           ldi     2                   ; high byte of 512
           phi     r7                  ; place into count
           ldi     0                   ; low byte of 512
           plo     r7                  ; place into low of count
           ldi     0                   ; need data register
           str     r2                  ; place on stack
           out     2                   ; select data register
           dec     r2                  ; move pointer
           sex     r6                  ; set data pointer
wrtloop:   out     3                   ; write to ide controller
           dec     r7                  ; decrement byte count
           glo     r7                  ; check for completion
           bnz     wrtloop             ; jump if not
           ghi     r7                  ; need to check high byte
           bnz     wrtloop             ; jump if more to go
           mark                        ; save current P and X
           sep     rf                  ; call waitrdy routine
           dec     r2                  ; compensate for RET increment
           lbr     return              ; and return to caller

rdide:     ldi     high waitrdy        ; get address of subs
           phi     rf                  ; place into rf
           ldi     low waitrdy
           plo     rf
           sex     r2                  ; point x to stack
           mark                        ; save current P and X
           sep     rf                  ; call wait for RDY routine
           dec     r2                  ; compensate for RET increment
           ldi     020h                ; command for sector read
           mark                        ; save current P and X
           sep     rf                  ; now call command sequence
           dec     r2                  ; compensate for RET increment
           ldi     2                   ; high byte of 512
           phi     r7                  ; place into count
           ldi     0                   ; lo byte of 512
           plo     r7                  ; place into low of count
           str     r2                  ; place on stack
           out     2                   ; select data register
           dec     r2                  ; move pointer
           sex     r6                  ; set data pointer
rdloop:    inp     3                   ; read from ide controller
           inc     r6                  ; point to next position
           dec     r7                  ; decrement byte count
           glo     r7                  ; check for completion
           bnz     rdloop              ; jump if not
           ghi     r7                  ; need to check high byte
           bnz     rdloop              ; jump if more to go
           ldi     0                   ; signify read complete
           lbr     return              ; and return to caller

          
beforerdy: irx                         ; move pointer to SAV location
           ret                         ; and return to caller
waitrdy:   sex     r2
           ldi     07h                 ; need status register
           str     r2                  ; store onto stack
           out     2                   ; write ide selection port
           dec     r2                  ; point x back to free spot
rdyloop:   inp     3                   ; read status port
           ani     0c0h                ; mask for BSY and RDY
           smi     040h                ; want only RDY bit
           bnz     rdyloop             ; loop back until drive is ready
           ldn     r2                  ; get status byte
           irx                         ; move pointer to SAV location
           ret                         ; and return to caller
; RF will point to wrtcmd, which is next needed after first waitrdy
wrtcmd:    sex     r2                  ; set x register
           stxd                        ; write passed command to stack
           ldi     7                   ; command register
           stxd                        ; write to stack
           ghi     r8                  ; get device
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
           str     r2                  ; write to stack
           out     2                   ; select sector count register
           out     3                   ; write sector count
           out     2                   ; select lba lo register
           out     3                   ; write low of lba
           out     2                   ; select lba mid register
           out     3                   ; write mid of lba
           out     2                   ; select lba high register
           out     3                   ; write high of lba
           out     2                   ; select head/device register
           out     3                   ; write device
           out     2                   ; select command register
           out     3                   ; write the write command
           dec     r2                  ; point back to free register
drqloop:   inp     3                   ; read status register
           ani     8                   ; mask for DRQ bit
           bz      drqloop             ; loop until found
           br      beforerdy           ; return, readying waitrdy again
; the branch to beforerdy, allows us to use waitrdy again

