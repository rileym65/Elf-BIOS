data:   equ     0
scall:  equ     r4
sret:   equ     r5

           org     0ff00h
f_boot:    lbr     boot
f_type:    lbr     type
f_read:    lbr     read
f_msg:     lbr     typemsg
f_typex:   lbr     return
f_input:   lbr     input
f_strcmp:  lbr     strcmp
f_ltrim:   lbr     ltrim
f_strcpy:  lbr     strcpy
f_memcpy:  lbr     memcpy
f_wrtsec:  lbr     wrtsec
f_rdsec:   lbr     rdsec
f_seek0:   lbr     trk0
f_seek:    lbr     seek
f_drive:   lbr     drvsel
f_setbd:   lbr     timalc
f_mul16:   lbr     mul16
f_div16:   lbr     div16
f_iderst:  lbr     resetide
f_idewrt:  lbr     wrtide
f_ideread: lbr     rdide
f_initcall: lbr    initcall
f_ideboot: lbr     bootide

return:    sep     sret                ; return to caller

; ***************************************
; *** Type message pointed to by R[F] ***
; ***************************************
typemsg:   lda     rf                  ; load byte from message
           lbz     return              ; return if last byte
           sep     scall               ; call type routine
           dw      f_type
           br      typemsg             ; loop until a zero found

input:     glo     ra                  ; save RA
           stxd
           ghi     ra
           stxd
           ldi     0                   ; byte count
           plo     ra                  ; store into counter
inplp:     sep     scall               ; call input function
           dw      f_read
           str     rf                  ; store into output
           inc     rf                  ; point to next position
           smi     08                  ; look for backspace
           bnz     nobs                ; jump if not a backspace
           glo     ra                  ; get input count
           bz      inplp               ; disregard if string is empty
           dec     ra                  ; decrement the count
           dec     rf                  ; decrement buffer position
           dec     rf
           br      inplp               ; and loop back for more
nobs:      smi     05                  ; check for CR
           bz      inpdone             ; loop back if not
           inc     ra                  ; increment input count
           br      inplp               ; and then loop back
inpdone:   ldi     0                   ; need a zero terminator
           str     rf                  ; store into buffer
           irx                         ; recover RA
           ldxa
           phi     ra
           ldx
           plo     ra
           sep     sret                ; return to caller


; *****************************************************
; *** Function to implement a stack based call      ***
; ***    RX is assumed to be the stack pointer      ***
; ***    R3 is assumed to be the main PC            ***
; ***    R6 will hold return address                ***
; ***    usage is:    sep R4                        ***
; ***                 dw  call_addr                 ***
; *****************************************************
         org     0ffdfh
         sep     r3                    ; jump to called routine
call:    plo     re                    ; Save D
         ghi     r6                    ; save last R[6] to stack
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
         irx                           ; point to old R[6]
         ldxa
         plo     r6
         ldx
         phi     r6
         glo     re
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
         sep     sret        ; return status code

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
         sep     sret        ; return to caller

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
divst:     glo     rd                  ; get low of divisor
           bnz     divgo               ; jump if still nonzero
           ghi     rd                  ; check hi byte too
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
; **********************************
; *** D = Character to output    ***
; *** RE.1 = Baud constant       ***
; **********************************
termxit: irx
        ldxa                 ; recover RD
        phi     rd
        ldxa
        plo     rd
        ldxa                 ; recover RC
        phi     rc
        ldx
        plo     rc
        sep     sret         ; return to caller
type:   plo     re
        glo     rc           ; save RC
        stxd
        ghi     rc
        stxd
        glo     rd           ; save RD
        stxd
        ghi     rd
        stxd
        glo     re           ; recover character
        phi     char
        xri     0ah          ; check for linefeed
        bnz     ty2          ; jump if not
        ldi     5bh          ; more bits if need to wait
        lskp                 ; jump
ty2:    ldi     0bh          ; 11 bits to write
        plo     char
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
        dec     char         ; decrement number of bits
        sdi     0
        ghi     char         ; get next bit of character
        shrc                 ; lease significant bit first
        phi     char         ; put back
        lsdf                 ; long skip if bit =0
        seq                  ;   set q=1 - "space"
        lskp                 ; skip next 2 bytes
        req                  ; if bit = 1,
        nop                  ;    set q=0 = "mark"
        glo     char         ; until #bits = 0
        ani     0fh
        bnz     nxtbit       ; loop if more to go
        glo     char         ; get code byte
        adi     0fbh         ; decrement code
        plo     char         ; set #bits
        bnf     termxit      ; jump if no more
        smi     01bh         ; if code = 1
        bz      termxit      ; then was last null, exit
zer:    ldi     0            ; if code > 1
        phi     char         ; load byte
        br      begin        ; and type it

delext: sep     r3           ; return from delay routine
delay1: ghi     baud         ; get baud constant
        shr                  ; remove echo flag
        plo     baud         ; repeat
dellp2: dec     baud         ;   decrement baud
        lda     r3           ;   get #bits
dellp1: smi     1            ;   decrement until zero
        bnz     dellp1       ;
        glo     baud         ;   until baud = 0
        bz      delext       ; return if done
        dec     r3
        br      dellp2

; **************************************
; *** RE.1 = Baud constant           ***
; *** Returns: RD.1 - read character ***
; **************************************
rexit:  ghi     rd           ; get character
        plo     re           ; save through restore
        irx
        ldxa                 ; recover RD
        phi     rd
        ldxa
        plo     rd
        ldxa                 ; recover RC
        phi     rc
        ldx
        plo     rc
        glo     re           ; recover character
        sep     sret         ; return to caller
read:   glo     rc           ; save RC
        stxd
        ghi     rc
        stxd
        glo     rd           ; save RD
        stxd
        ghi     rd
        stxd
        ldi     0            ; flag for terminal control
        plo     char         ; save entry flag
read2:  ldi     80h          ; set #bits in character = 7
        phi     char         ; (7 shift changes 80 into 01)
        ldi     delay1.1
        phi     delay
        ldi     delay1.0
        plo     delay
;        sex     rb
;        out     7            ; turn reader on
;        db      80h
        bn4     $            ; wait while stop bit
tty1:   b4      $
        sep     delay        ; delay 1/2 bit time
        db      2
        b4      tty1
;        out     7
;        db      40h
nobit:  inc     r2           ; equalize delays
        dec     r2
bit:    ghi     baud
        shr
        bdf     noecho       ; check if need to echo
        b4      outbit
        seq                  ; set q if bit = 1
        lskp                 ; reset q if bit=0
outbit: req
noecho: nop                  ; equalize delays
        lsnf
        nop 
        nop
        nop
        nop
        sep    delay         ; wait 1 bit time
        db     7
        inc    baud          ; set delay flag = 1
        ghi    char          ; shift by 1 bit
        shr
        phi    char 
        bdf    stop
        ori    80h
        bn4    nobit
        phi    char          ; continue loop
        br     bit
stop:   req                  ; set stop bit
        bz     read2         ; repeat if 00=null
        br     rexit         ; done

; ************************************
; *** Determine terminal baud rate ***
; *** Waits for CR or Lf           ***
; *** Returns RE.1 - Baud constant ***
; ************************************
timalc:  ldi   high delay1
         phi   delay
         ldi   low delay1
         plo   delay
         ldi   0
         plo   baud
         plo   char 
         b4    $
         bn4   $
         ldi   3
tc:      smi   1
         bnz   tc
         glo   char 
         bnz   zto1
         b4    incr
         inc   char 
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
         sep     sret  

initcall:  ldi     high ret
           stxd
           stxd
           phi     r5
           ldi     low ret
           plo     r5
           ldi     high call
           phi     r4
           ldi     low call
           plo     r4
           sep     r5

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
           sep     sret                ; return to caller

           
wrtide:    ldi     high waitrdy        ; get address of subs
           phi     rc                  ; place into rf
           ldi     low waitrdy
           plo     rc
           sex     r2                  ; point x to stack
           mark                        ; save current P and X
           sep     rc                  ; call wait for RDY routine
           dec     r2                  ; compensate for RET increment
           ldi     030h                ; command for sector write
           mark                        ; save current P and X
           sep     rc                  ; now call command sequence
           dec     r2                  ; compensate for RET increment
           ldi     2                   ; high byte of 512
           phi     r7                  ; place into count
           ldi     0                   ; low byte of 512
           plo     r7                  ; place into low of count
           ldi     0                   ; need data register
           str     r2                  ; place on stack
           out     2                   ; select data register
           dec     r2                  ; move pointer
           sex     rf                  ; set data pointer
wrtloop:   out     3                   ; write to ide controller
           dec     r7                  ; decrement byte count
           glo     r7                  ; check for completion
           bnz     wrtloop             ; jump if not
           ghi     r7                  ; need to check high byte
           bnz     wrtloop             ; jump if more to go
           sex     r2                  ; point x to stack
           mark                        ; save current P and X
           sep     rc                  ; call waitrdy routine
           dec     r2                  ; compensate for RET increment
           sep     sret                ; and return to caller

rdide:     ldi     high waitrdy        ; get address of subs
           phi     rc                  ; place into rf
           ldi     low waitrdy
           plo     rc
           sex     r2                  ; point x to stack
           mark                        ; save current P and X
           sep     rc                  ; call wait for RDY routine
           dec     r2                  ; compensate for RET increment
           ldi     020h                ; command for sector read
           mark                        ; save current P and X
           sep     rc                  ; now call command sequence
           dec     r2                  ; compensate for RET increment
           ldi     2                   ; high byte of 512
           phi     r7                  ; place into count
           ldi     0                   ; lo byte of 512
           plo     r7                  ; place into low of count
           str     r2                  ; place on stack
           out     2                   ; select data register
           dec     r2                  ; move pointer
           sex     rf                  ; set data pointer
rdloop:    inp     3                   ; read from ide controller
           inc     rf                  ; point to next position
           dec     r7                  ; decrement byte count
           glo     r7                  ; check for completion
           bnz     rdloop              ; jump if not
           ghi     r7                  ; need to check high byte
           bnz     rdloop              ; jump if more to go
           ldi     0                   ; signify read complete
           sex     r2
           sep     sret                ; and return to caller

          
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
wrtcmd:    sex     r2
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

bootide:   ldi     high bootret        ; prepare for seetin call
           phi     r6
           ldi     low bootret
           plo     r6
           lbr     f_initcall
bootret:   sep     scall               ; reset ide drive
           dw      f_iderst
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
           ldi     1
           phi     r0
           ldi     0
           plo     r0
           sep     r0

