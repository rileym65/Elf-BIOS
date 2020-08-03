; *******************************************************************
; *** This software is copyright 2004 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

; #define ELF2K
#define SERP    b2
#define SERN    bn2

data:   equ     0
scall:  equ     r4
sret:   equ     r5

           org     0f800h
; *** Check if character is numeric
; *** D - char to check
; *** Returns DF=1 if numeric
; ***         DF=0 if not
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

; *** rf - pointer to ascii string
; *** returns: rf - first non-numeric character
; ***          RD - number
; ***          DF = 1 if first character non-numeric 
atoi:      ldi     0                   ; clear answer
           phi     rd
           plo     rd
           ldn     rf                  ; get first value
           sep     scall               ; check if numeric
           dw      isnum
           bdf     atoicnt             ; jump if so
           xri     '-'                 ; check for minus
           bz      atoicnt             ; jump if so
           ldi     1                   ; signal number error
           shr
           sep     sret                ; return to caller
atoicnt:   sex     r2                  ; make sure x points to stack
           glo     rc                  ; save consumed registers
           stxd
           ghi     rc
           stxd
           glo     re
           stxd
           plo     re                  ; signify positive number
           ldn     rf                  ; get first bytr
           xri     '-'                 ; check for negative
           bnz     atoilp              ; jump if not negative
           ldi     1                   ; signify negative number
           plo     re
           inc     rf                  ; move past - sign
atoilp:    ldn     rf                  ; get byte from input
           smi     '0'                 ; convert to binary
           bnf     atoidn              ; jump if below numbers
           smi     10                  ; check for above numbers
           bdf     atoidn              ; jump if above numbers
           glo     rd                  ; multiply by 2
           plo     rc                  ; keep a copy
           shl
           plo     rd
           ghi     rd
           phi     rc
           shlc
           phi     rd
           glo     rd                  ; multiply by 4
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           glo     rc                  ; multiply by 5
           str     r2
           glo     rd
           add
           plo     rd
           ghi     rc
           str     r2
           ghi     rd
           adc
           phi     rd
           glo     rd                  ; multiply by 10
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
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
atoidn:    nop
           irx                         ; recover consumed registers
           ldxa
           plo     re
           ldxa
           phi     rc
           ldx
           plo     rc
           ldi     0                   ; signal valid number
           shr
           sep     sret                ; return to caller

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

numbers:   db      027h,010h,3,0e8h,0,100,0,10,0,1


           org     0f900h
buffer:    equ     03
minimon:   ldi     high start          ; setup main pc
           phi     r6
           ldi     low start
           plo     r6
           ldi     0                   ; setup stack
           phi     r2
           ldi     0ffh
           plo     r2
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
           smi     28                  ; look for copy command
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
           dw      f_type
           br      typeinmsg           ; loop until a zero found


           org   0fa00h
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

;
; MOVER.ASM - Function Move Programs Into Low memory
; For Execution.
;
; Normal Entry PC = 3, RF = Program Header
;
; Exit: Starts Program at specified address
; With P=0,X=0,IE=1
;
; This module written by Richard Peters
;*********************************************************
; HEADER DEFINITION ANY ADDRESS
; OFFSET
;  00   = Program RAM Start Address
; +02   = Program RAM End Address
; +04   = Program Execution Address
; +06   = Accual PROGRAM Bytes
;*********************************************************
           org   0fadah
mover:     sex     r0
           lda     rf
           phi     r0
           lda     rf
           plo     r0
           lda     rf
           phi     r1
           lda     rf
           plo     r1
           inc     r1
           lda     rf
           phi     ra
           lda     rf
           plo     ra
moverlp:   lda     rf
           str     r0
           inc     r0
           glo     r1
           str     r0
           glo     r0
           sd
           bnz     moverlp
           ghi     r1
           str     r0
           ghi     r0
           sd
           bnz     moverlp
           ldi     3
           str     r0
           ret
           ghi     ra
           phi     r0
           glo     ra
           plo     r0
           sep     r0

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
           dec     r2                  ; compensate for ret increment
           sep     sret                ; return to caller

           
wrtide:    glo     rc                  ; save consumed registers
           stxd
           ghi     rc
           stxd
           glo     r7
           stxd
           ghi     r7
           stxd
           ldi     high waitrdy        ; get address of subs
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
ideret:    plo     re
           irx                         ; recover consumed registers
           ldxa
           phi     r7
           ldxa
           plo     r7
           ldxa
           phi     rc
           ldx
           plo     rc
           glo     re
           sep     sret                ; and return to caller

rdide:     glo     rc                  ; save consumed registers
           stxd
           ghi     rc
           stxd
           glo     r7
           stxd
           ghi     r7
           stxd
           ldi     high waitrdy        ; get address of subs
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
           br      ideret              ; return to caller
          
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


	org	0fc00h
           sep     r3
delay:     ghi     re                  ; get baud constant
           shr                         ; remove echo flag
           plo     re                  ; put into counter
           sex     r2                  ; waste a cycle
delay1:    dec     re                  ; decrement counter
           glo     re                  ; get count
           bz      delay-1             ; return if zero
           br      delay1              ; otherwise keep going

timalc:    glo     rb                  ; save consumed registesr
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
           SERP    $                   ; wait until start bit found
setbd1:    inc     rc
           sex     r2
           sex     r2
           SERN    setbd1              ; wait until another high
setbd2:    inc     rb
           sex     r2
           sex     r2
           SERP    setbd2              ; wait til the next low
           glo     rb                  ; compare values
           shr                         ; quantize over small differences
           shr     
           str     r2
           glo     rc
           shr
           shr
           sm      
           bz      setbd3             ; jump if CR was entered
           glo     rb
           plo     rc
           ldi     0
           lskp    
setbd3:    ldi     1
           phi     rb
           glo     rc
           smi     4
           phi     re
           ghi     rb
           shr     
           ghi     re
           shlc    
           phi     re
           irx                         ; recover consumed registesr
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
           glo     rf
           stxd
           ghi     rf
           stxd
           glo     rd
           stxd
           ghi     rd
           stxd
           glo     re
           phi     rf
           ldi     9                   ; 9 bits to send
           plo     rf
           ldi     high delay
           phi     rd
           ldi     low delay
           plo     rd
           ldi     0
           shr
sendlp:    bdf     sendnb              ; jump if no bit
           seq                         ; set output
           br      sendct
sendnb:    req                         ; reset output
           br      sendct
sendct:    sep     rd                  ; perform bit dela
           sex r2
           sex r2
           ghi     rf
           shrc
           phi     rf
           dec     rf
           glo     rf
           bnz     sendlp
           req                         ; set stop bits
           sep     rd
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rf
           ldx
           plo     rf
           sep     sret

#ifdef ELF2K
read:      glo     rf
           stxd
           ghi     rf
           stxd
           glo     rd
           stxd
           ghi     rd
           stxd
           ldi     8                   ; 8 bits to receive
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
recvdone:  req
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
           glo     re
           shr
           plo     re                  ; save char
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
read:      glo     rf
           stxd
           ghi     rf
           stxd
           glo     rd
           stxd
           ghi     rd
           stxd
           ldi     8                   ; 8 bits to receive
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
recvdone:  req
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
           glo     re
           shr
           sep     sret                ; and return to caller
recvlp0:   br      recvlp1             ; equalize between 0 and 1

recvlpe:   ghi     rf
           shr                         ; shift right
           SERN    recvlpe0            ; jump if zero bi
           ori     128                 ; set bit
           req
recvlpe1:  phi     rf
           sep     rd                  ; perform bit delay
           dec     rf                  ; decrement bit count
           sex     r2
           sex     r2
           glo     rf                  ; check for zero
           bnz     recvlpe             ; loop if not
           br      recvdone
recvlpe0:  seq
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
ttyend:    lbr     type                ; and display character
ttyff:     sep     scall               ; display vt100 sequence to clear screen
           dw      typeinmsg
           db      01bh,'[2J',0
           sep     sret                ; and return to caller


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

         org     0fe00h
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
           irx                         ; recover RF
           ldxa
           phi     rf
           ldx
           plo     rf
           sep     sret                ; and return to caller
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
freemem:   ldi     0         ; start from beginning of memory
           phi     rf        ; place into register
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
           inc     rf        ; point to next position
           glo     rf        ; check for possible wrap around
           bnz     fmemlp    ; jump if not
           ghi     rf
           bnz     fmemlp
fmemdn:    dec     rf        ; point back to last writable memory
           sep     sret      ; and return to caller

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

           org     0ff00h
f_boot:    lbr     bootide
f_type:    lbr     tty
f_read:    lbr     read
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
f_ideboot: lbr     bootide
f_hexin:   lbr     hexin
f_hexout2: lbr     hexout2
f_hexout4: lbr     hexout4
f_tty:     lbr     type
f_mover:   lbr     mover
f_minimon: lbr     minimon
f_freemem: lbr     freemem
f_isnum:   lbr     isnum
f_atoi:    lbr     atoi
f_uintout: lbr     uintout
f_intout:  lbr     intout
f_inmsg:   lbr     typeinmsg
f_inputl:  lbr     input
f_brktest: lbr     brktest
f_findtkn: lbr     tokenfunc
f_isalpha: lbr     isalpha
f_ishex:   lbr     ishex
f_isalnum: lbr     isalnum
f_idnum:   lbr     idnum
f_isterm:  lbr     isterm

return:    sep     sret                ; return to caller

; ***************************************
; *** Type message pointed to by R[F] ***
; ***************************************
typemsg:   lda     rf                  ; load byte from message
           lbz     return              ; return if last byte
           sep     scall               ; call type routine
           dw      f_type
           br      typemsg             ; loop until a zero found

input256:  ldi     1                   ; allow 256 input bytes
           phi     rc
           ldi     0
           plo     rc
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
           dw      type
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
bs2:       ldi     32                  ; display a space
           sep     scall
           dw      type
           ldi     8                   ; then backspace again
           sep     scall
           dw      type
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
inpterm:   ldi     1                   ; signal <CTRL><C> exit
           shr
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
         org     0ffe0h
         lbr     call
         org     0fff1h
         lbr     ret

         org     0fff9h
version: db      0,9,4
chsum:   db      0,0,0,0

