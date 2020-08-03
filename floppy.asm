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
