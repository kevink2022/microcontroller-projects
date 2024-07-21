***********************************************************************
*
* Title:          SCI Serial Port and 7-segment Display at PORTB
*
* Objective:      CMPEN 472 Homework 5 Submission
*
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Sep. 27, 2021
*
* Programmer:     Kevin Kelly
*
* Company:        The Pennsylvania State University
*                 Department of Computer Science and Engineering
*
* Program:        Simple SCI Serial Port I/O and Demonstration
*                 Typewriter program and 7-Segment display, at PORTB
*                 
*
* Algorithm:      Simple Serial I/O use, typewriter
*
* Register use:	  A: Serial port data
*                 X,Y: Delay loop counters
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
* Output:         
*                 PORTB bit 7 to bit 4, 7-segment MSB
*                 PORTB bit 3 to bit 0, 7-segment LSB
*
* Observation:    This is a typewriter program that displays ASCII
*                 data on PORTB - 7-segment displays.
*
***********************************************************************
* Parameter Declearation Section
*
* Export Symbols
            XDEF        pstart       ; export 'pstart' symbol
            ABSENTRY    pstart       ; for assembly entry point
  
* Symbols and Macros
PORTB       EQU         $0001        ; i/o port B addresses
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character
BS          equ         $08          ; ASCII 'Backspace' key

***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address 
                                     ;   for Data for CMPEN 472 class
Counter1      DC.W      $0006        ; X register count number for time delay
                                     ;   inner loop for msec (Stored: $3000 - $3001)
Counter2      DC.W      $0001        ; Y register count number for time delay
                                     ;   outer loop for sec
LEDop         DC.B      $FF          ; Modified depending on the command executed then
                                     ;   used to modify LED lights
fadeControl   DC.W      $6464        ; Used to control the number of cycles for LEDfade 
                                     ;   high byte modified between cycles, low bye
                                     ;   is always total of cycles per loop, in this case 100
inputpointer  DS.B      3            ; The bytes reserved in memory for the typed commands to 
                                     ;   to be stored

                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start

*
***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*
            ORG        $3100        ; Program start address, in RAM
pstart      LDS        #$3100       ; initialize the stack pointer

            LDAA       #%11111111   ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA       DDRB         ; as output

            LDAA       #%00000000
            STAA       PORTB        ; clear all bits of PORTB

            ldaa       #$0C         ; Enable SCI port Tx and Rx units
            staa       SCICR2       ; disable SCI interrupts

            ldd        #$0001       ; Set SCI Baud Register = $0001 => 2M baud at 24MHz (for simulation)
;            ldd        #$0002       ; Set SCI Baud Register = $0002 => 1M baud at 24MHz
;            ldd        #$000D       ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;            ldd        #$009C       ; Set SCI Baud Register = $009C => 9600 baud at 24MHz
            std        SCIBDH       ; SCI port baud rate change
      
            ldy   #inputpointer     ; initiliaze the pointer for commands to be stored
            ldaa  #$00
            
initloop                            
            staa  1,Y+              ; Loop fills memory space for commands with
            cpy   #$3010            ;   $00 to initialize
            bne   initloop
            
menuprint
            ldy   #inputpointer     ; Initialize pointer for command input       
            jsr   printmenu         ; Prints the menu to the terminal

inputloop       
            jsr   getchar           ; check the key board for characters
            cmpa  #$00              ;  if nothing typed, keep checking
            beq   inputloop
                                    ;  otherwise - what is typed on key board    
            cpy   #$300E            ; check the pointer to ensure commands are limited to   
            beq   charlimit         ;   5 charactes or less                  
            staa  1,Y+
            jsr   putchar           ; displays typed character on terminal

charlimit
            cmpa  #CR               ; Checks if the Enter/Return key was typed
            bne   inputloop         ;   if so, continue to execute the next command.
            jsr   execute           ; Checks if command is valid then executes command.
            ldx   #cmdmsg           ; Prompt another command after command execution
            jsr   printline
            bra   inputloop         ; brach back to recieve new command
            
typewriter                          ; Typewriter loops forever once entered.
            jsr   getchar           ; type writer - check the key board
            cmpa  #$00              ;  if nothing typed, keep checking
            beq   typewriter
                                    ;  otherwise - what is typed on key board
            jsr   putchar           ; is displayed on the terminal window - echo print

            staa  PORTB             ; show the character on PORTB

            cmpa  #CR
            bne   typewriter        ; if Enter/Return key is pressed, move the
            ldaa  #LF               ; cursor to next line
            jsr   putchar           
            bra   typewriter
            
;subroutine section below

;***********execute****************************
;* Program: Validates and Executes commands in memory.
;* Input:   Register Y points to ASCII characters in memory.
;* Output:  LED control or message printed on the terminal 
;*          connected to SCI port.
;* 
;* Registers modified: CCR, Y, A
;* Algorithm:
;     Check first character of command, continue execution if valid.
;     Check second character of command, branch to either execution
;       if a 2 char command, or check the next 2 char for quit command.
;     If command is invalid, print warning to terminal and return.
;**********************************************
execute     ldy   #inputpointer      ; load Y with input pointer
            ldaa  1,Y+               ; load A with 1st char of input
            cmpa  #$4C
            beq   l_exe              ; branch if 'L'
            cmpa  #$6C
            beq   l_exe              ; branch if 'l'
            cmpa  #$46               
            beq   f_exe              ; branch if 'F'
            cmpa  #$66
            beq   f_exe              ; branch if 'f'
            cmpa  #$51
            beq   q_exe              ; branch if 'Q'
            cmpa  #$71
            beq   q_exe              ; branch if 'u'
            
            bra   invalid            ; if none of above, invalid command
            
            
            
l_exe                                ; check for valid L or F command
f_exe
           ldaa 1,Y+                 ; load A with 2nd char of input
           cmpa #$31
           beq  LEDcommand           ; branch if '1'
           cmpa #$32
           beq  LEDcommand           ; branch if '2'
           cmpa #$33
           beq  LEDcommand           ; branch if '3'
           cmpa #$34
           beq  LEDcommand           ; branch if '4'
           bra  invalid              ; if none of above, invalid command
           
LEDcommand                           ; control the LED based on valid
           ldaa  1,Y+                ;   valid L or F command 
           cmpa  #CR                 ; check for extra characters in command 
           bne   invalid             ;   if present, command if invalid
           jsr   LEDcontrol          ; execute LED control based off command
           bra   reset 
 
q_exe                                ; check for valid QUIT command
           ldaa  1,Y+
           cmpa  #$55
           beq   u_valid             ; continue if 'U'
           cmpa  #$75
           beq   u_valid             ; continue if 'u'
           bra   invalid             ; if none of above, invalid command
u_valid           
           ldaa  1,Y+
           cmpa  #$49
           beq   i_valid             ; continue if 'I'
           cmpa  #$69
           beq   i_valid             ; continue if 'i'
           bra   invalid             ; if none of above, invalid command
i_valid           
           ldaa  1,Y+
           cmpa  #$54
           beq   t_valid             ; continue if 'T'
           cmpa  #$74
           beq   t_valid             ; continue if 't'
           bra   invalid             ; if none of above, invalid command
t_valid           
                                     ; execute quit command
           ldx   #msg1               ; Print greeting message of typewriter
           jsr   printline
           ldx   #msg2
           jsr   printline
           lbra  typewriter          ; Branch to typewriter
           
 
invalid                              ; print invalid command message
           ldx   #invalidmsg         
           jsr   printline
           
reset                                ; fill command memory with $00 to reset
           ldy   #inputpointer       
           ldaa  #$00
rstloop    
           staa  1,Y+
           cpy   #$3010
           bne   rstloop
           
           ldy   #inputpointer       ; reset Y to input pointer    
           rts           
;****************end of execute****************

;***********LEDcontrol*************************
;* Program: Change LED state based off of command.
;* Input:   Register Y points to ASCII characters in memory.
;* Output:  Change in LED state
;* 
;* Registers modified: CCR, D, Y
;* Algorithm:
;     Modify the number from the command to the 
;       correct bytes to modify the LED
;     Check if an 'L' or 'F' command to turn ON/OFF LED
;     If it is an 'X2' command, us the LEDfade
;       subroutine to fade in LED in or out
;     Else, change LED state based off of command.
;**********************************************
LEDcontrol  pshy
            pshd
            ldy   #inputpointer
            
            ldab  1,Y                ; load B with LED num ascii
            andb  #$0F               ; discard first halfbyte 
            ldaa  #$04
            
shiftloop                            ; shift LED num by 4
            lslb                     ;   $01 -> $10 (Correct)
            deca                     ;   $02 -> $20 (Correct)
            bne   shiftloop          ;   $03 -> $30 (Incorrect, need $40)
                                     ;   $04 -> $40 (Incorrect, need $80)
            
            cmpb  #$40               ; if LED is 4, left shift
            bne   not4               ;   $40 -> $80 (Correct)
            lslb
not4        cmpb  #$30               ; if LED is 3, add 1
            bne   not3               ;   $30 -> $40 (Correct)
            addb  #$10            
not3        stab  LEDop              ; store B in LEDop to control LED
            
            ldaa  Y                  ; load A with LED letter ascii
            anda  #$0F
            
            cmpb  #$20               ; if LED is 2, branch to fade control
            beq   LED2
                                         
            cmpa  #$06               ;    if 'L', branch to LEDon
            bne   LEDon              ;    if 'F', continue to LEDoff

LEDoff      ldaa  LEDop              ; load A with LEDop 
            eora  #$F0               ; complement LEDop
            staa  LEDop              ; store LEDop
            ldaa  PORTB              ; load A with PORTB 
            anda  LEDop              ; and PORTB with LEDop to modify
            staa  PORTB              ; store modified PORTB
            bra   exit
                                    
LEDon       ldaa  PORTB              ; load A with PORTB
            oraa  LEDop              ; or PORTB with LEDop to modify
            staa  PORTB              ; store modified PORTB
            bra   exit
                                     
LED2        staa  LEDop              ; LEDop isn't used the same way here as above
                                     ;   instead it determines if the command is L2 or F2
                                     ;   allowing the fadeControl subroutine to determine
                                     ;   to fade in or out
            ldd   fadeControl
LED2Loop    jsr   LEDfade
            dec   fadeControl
            bne   LED2Loop
            
            ldaa  #$64
            staa  fadeControl

exit        puly
            puld
            rts
;****************end of LEDControl*************

;***********LEDfade****************************
;* Program: Fades LED2 in or out depending on LEDop in memory
;* Input:   LEDop in memory and fadecontrol in memory
;* Output:  LED2 state change
;* 
;* Registers modified: CCR, A
;* Algorithm:
;     Check LEDop to determine fade IN/OUT
;     Set initial LED2 state
;     Loop fadecontrol HIGH BYTE times
;     Invert LED2 state
;     Loop fadecontrol LOW BYTE - HIGH BYTE times
;**********************************************
LEDfade
            LDAA      LEDop           ; If LEDop == $06, it is the 'F2' command, 
            CMPA      #$06            ;   so the LED needs to start ON to fade down
            BEQ       ALoopOn
            
            BCLR      PORTB,#$20      ; Otherwise, its an 'L2' command, so LED needs
            BRA       ALoopOff        ;   to start OfF to fade up

ALoopOn     BSET      PORTB,#$20

ALoopOff                              ; ALoopOff just used to skip ALoopOn if loop is off
            
            LDD       fadeControl
            CMPA      #$0
            BEQ       skipA           ; Skip delay loop if A = 0
            
fadeLoopA   JSR       delay400us      ; Delay based off of DCControl high byte 
            DECA
            BNE       fadeLoopA
skipA            
            LDAA      LEDop           ; If LEDop == $06, it is the 'F2' command,
            CMPA      #$06            ;   so the LED needs to end OFF to fade down
            BEQ       BLoopOff
            
            BSET      PORTB,#$20      ; Otherwise, its an 'L2' command, so LED needs
            BRA       BLoopOn         ;   to end ON to fade up

BLoopOff    BCLR      PORTB,#$20

BLoopOn            
            
            LDD       fadeControl
            CMPA      #$64            ; B = low byte - high byte 
            BEQ       skipB           ; Skip delay if B=0
            
fadeLoopB   JSR       delay400us      ; Delay based off of the difference between 
            DECB                      ;   DCControl low and high bytes
            BNE       fadeLoopB 
skipB            
            RTS             
;****************end of LEDfade****************

;***********printmenu**************************
;* Program: Prints the menu of available commands
;* Input:   Command menu in memory
;* Output:  Command menu printed on terminal
;* 
;* Registers modified: CCR, X
;* Algorithm:
;     Load and then print each line of the menu.
;**********************************************
printmenu   
             
            ldx   #menu1             
            jsr   printline
            ldx   #menu2
            jsr   printline
            ldx   #menu3
            jsr   printline
            ldx   #menu4
            jsr   printline
            ldx   #menu5
            jsr   printline
            ldx   #menu6
            jsr   printline
            ldx   #menu7
            jsr   printline
            ldx   #menu8
            jsr   printline
            ldx   #menu9
            jsr   printline
            ldx   #cmdmsg
            jsr   printline

            rts
;****************end of printmenu**************            

;***********printline**************************
;* Program: Prints the characters loaded into the X register
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR, A
;* Algorithm:
;     Jump to printmsg subroutine
;     Move to the next line on the terminal
;**********************************************
printline   
            jsr   printmsg
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            rts
;****************end of printline**************

;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL           equ     $00
printmsg       psha                   ;Save registers
               pshx
printmsgloop   ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
               cmpa    #NULL
               beq     printmsgdone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx 
               pula
               rts
;***********end of printmsg********************

;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar        brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
               staa  SCIDRL                      ; send a character
               rts
;***************end of putchar*****************


;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************
getchar        brclr SCISR1,#%00100000,getchar7
               ldaa  SCIDRL
               rts
getchar7       clra
               rts
;****************end of getchar****************

;***********delay400us*************************
;* Program: Delays program execution by 400 us
;* Input:   Register Y points to number of loops in memory
;* Output:  Execution delay/time waste
;* 
;* Registers modified: CCR, Y
;* Algorithm:
;     Load number of loops into Y register
;     Delay 10us each loop
;     Execute a few extra NOPs for exactly 400us
;**********************************************
delay400us
            PSHY                      ; save Y
            LDY   Counter2            ; long delay by 
            
dly400uLoop JSR   delay10us           ; total time delay = Y * delayMS
            DEY                      
            BNE   dly400uLoop
            
;            LDY   #$004A            
;extraLoop   DEY                       ; extra loop is present to get an 
;            BNE   extraLoop           ; even 9,600 CC for a 400us delay
;                                      ; NOTE: NOT USED HERE DUE TO 
;            NOP                       ;       SIMULATOR LAG
;            NOP
            PULY                      ; restore Y
            RTS                       ; return
;****************end of delay400us*************

;***********delay10us**************************
;* Program: Delays program execution by 10 us
;* Input:   Register X points to number of loops in memory
;* Output:  Execution delay/time waste
;* 
;* Registers modified: CCR, X
;* Algorithm:
;     Load number of loops into X register
;     Loop X times
;     Execute a few extra NOPs for exactly 10us
;**********************************************
delay10us
            PSHX                      ; save X
            LDX   Counter1            ; short delay 
            
dly10usLoop                           ; total time delay = X * NOP
            DEX                      
            BNE   dly10usLoop            

            PULX                      ; restore X
            NOP                       ; -3 No operations are added to get to 240 CC
            NOP                       ; -to take the number of CC's
            NOP                       ; -to a number divisble by 4.
            RTS                       ; return
;****************end of delay10us**************

 


;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip

; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program - before the last "END" line.

msg1           DC.B     'Hello', $00
msg2           DC.B     'You may type below', $00

cmdmsg         DC.B    'Enter your command below:', $00
invalidmsg     DC.B    'Error: Invalid command', $00

menu1          DC.B    'L1: Turn on LED1', $00
menu2          DC.B    'L1: Turn off LED1', $00
menu3          DC.B    'L2: LED 2 goes from 0% light level to 100% light level in 4 seconds', $00
menu4          DC.B    'F2: LED 2 goes from 100% light level to 0% light level in 4 seconds', $00
menu5          DC.B    'L3: Turn on LED3', $00
menu6          DC.B    'F3: Turn off LED3', $00
menu7          DC.B    'L4: Turn on LED4', $00
menu8          DC.B    'F4: Turn off LED4', $00
menu9          DC.B    'QUIT: Quit menu program, run Type writer program.', $00


               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled
