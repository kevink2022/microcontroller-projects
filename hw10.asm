***********************************************************************
*
* Title:          CMPEN 472 Homework 9 Submission
*
* Objective:      (1) To learn interrupts and timing: Real Time Interrupt (RTI). 
*                 (2) Also to learn interrupt based multi-tasking programming. 
*
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Dec. 2nd, 2021
*
* Programmer:     Kevin Kelly
*
* Company:        The Pennsylvania State University
*                 Department of Computer Science and Engineering
*
* Program:        Simple Terminal Calculator program
*                 60-second counter using RTI
*                 
* Algorithm:      Simple Serial I/O use and RTI
*
* Register use:	  A: Serial port data
*                 X,Y: Delay loop counters, address locations, mathematical operators
*		              D,A,B: Mathematical operators
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
* Output:         
*                 PORTB bit 7 to bit 4, 7-segment MSB
*                 PORTB bit 3 to bit 0, 7-segment LSB
*
* Observation:    This is a calculator program that displays ASCII
*                 data on Serial Port 2 (Terminal), along with a timer
*                 displayed on the 7 segment displays
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

ATDCTL2     EQU         $0082        ; Analog-to-Digital Converter (ADC) registers
ATDCTL3     EQU         $0083
ATDCTL4     EQU         $0084
ATDCTL5     EQU         $0085
ATDSTAT0    EQU         $0086
ATDDR0H     EQU         $0090
ATDDR0L     EQU         $0091
ATDDR7H     EQU         $009e
ATDDR7L     EQU         $009f

CRGFLG      EQU         $0037        ; Clock and Reset Generator Flags
CRGINT      EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL      EQU         $003B        ; Real Time Interrupt Control

TIOS        EQU         $0040        ; Timer Input Capture (IC) or Output Compare (OC) select
TIE         EQU         $004C        ; Timer interrupt enable register
TCNTH       EQU         $0044        ; Timer free runing main counter
TSCR1       EQU         $0046        ; Timer system control 1
TSCR2       EQU         $004D        ; Timer system control 2
TFLG1       EQU         $004E        ; Timer interrupt flag 1
TC6H        EQU         $005C        ; Timer channel 2 register

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character
space       equ         $20          ; ASCII for space
dollar      equ         $24          ; ASCII for '$'
nullchar    equ         $00          ; ASCII for a null character

***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address 
                                     ;   for Data for CMPEN 472 class
rti_count     DC.W      0            ; Value in memory that is incremented every RTI
                                     ;   to keep track of time for the 60 sec timer
time          DS.B      1            ; Value in memory that represents what the timer
                                     ;   is actually displaying
timermult     DC.W      160          ; Since simulator is very inaccurate, adjusting  
                                     ;   this number adjusts timer rate
wavelength    DC.W      1023         ; Number that once wave count equals, waveform ends

wavecount     DC.W      0            ; Counter for the timer int to increment and to 
                                     ;   to compare to wavelength for generating wave
analogsample  DS.B      1            ; Place in memory to store each analog sample                      
                                     
BUF           DS.B      6            ; character buffer for a 16bit number in decimal ASCII
CTR           DS.B      1            ; character buffer fill count

typespace     DS.B      7            ; The bytes reserved in memory for the typed commands to 
                                     ;   to be stored (limits input characters to this space)
breakspace    DS.B      2            ; Put 2 nullchar between typespace and printspace for
                                     ;   printing to the terminal
numspace      DS.B      7            ; The bytes reserved for the actual address and data
                                     ;   values to be stored
mathspace     DS.B      2            ; The bytes reserved for the result of 16-bit operations 
                                     
endspace      DS.B      1            ; Byte to mark the end of storage space                  

                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start
*
******************************************************************************
* Interrupt vector section
*
            ORG    $FFF0             ; RTI interrupt vector setup for the simulator
;            ORG    $3FF0             ; RTI interrupt vector setup for the CSM-12C128 board
            DC.W   rtiisr
            
            ORG     $FFE2       ; Timer channel 6 interrupt vector setup, on simulator
            DC.W    oc6isr

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
            
            ; ATD initialization
            LDAA  #%11000000       ; Turn ON ADC, clear flags, Disable ATD interrupt
            STAA  ATDCTL2
            LDAA  #%00001000       ; Single conversion per sequence, no FIFO
            STAA  ATDCTL3
            LDAA  #%10000111       ; 8bit, ADCLK=24MHz/16=1.5MHz, sampling time=2*(1/ADCLK)
            STAA  ATDCTL4          ; for SIMULATION
                        
menuprint                 
            ldy   #typespace        ; Initialize pointer for command input       
            jsr   printmenu         ; Prints the menu to the terminal
            jsr   reset
            
            bset   RTICTL,%00011001 ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                    ;      4MHz quartz oscillator clock
            bset   CRGINT,%10000000 ; enable RTI interrupt
            bset   CRGFLG,%10000000 ; clear RTI IF (Interrupt Flag)


            ldx    #0
            stx    rti_count        ; initialize interrupt counter with 0.
            cli                     ; enable interrupt, global

inputloop       
            
            jsr   getchar           ; check the key board for characters
            cmpa  #nullchar         ;  if nothing typed, keep checking
            beq   inputloop
                                    ;  otherwise - what is typed on key board    
            cpy   #breakspace       ; check the pointer to ensure commands are limited to   
            beq   charlimit         ;   5 charactes or less
            cmpa  #CR
            beq   charlimit         ; Don't place #CR ($0D) in typespace                   
            staa  1,Y+
            jsr   putchar           ; displays typed character on terminal

charlimit
            cmpa  #CR               ; Checks if the Enter/Return key was typed
            bne   inputloop         ;   if so, continue to execute the next command.
            ldy   #typespace
            jsr   validate          ; Checks if command is valid then executes command.
            bra   inputloop         ; brach back to recieve new command
            
typewriter                          ; Typewriter loops forever once entered.
            bclr  CRGFLG,%10000000  ; set RTI Interrupt Flag - to stop interrupts
twloop
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
            bra   twloop
                        
;subroutine section below

;***********RTI interrupt service routine***************
rtiisr      bset   CRGFLG,%10000000 ; clear RTI Interrupt Flag - for the next one
            CLI                     ; clear global interrupt flag for consistent sampling
            ldx    rti_count
            inx                     ; every time the RTI occur, increase
            
            cpx    timermult        ; check if time to tick the clock
            blo    donttick
            
            jsr    clocktick        ; tick the clock if RTI = timermult
            ldx    #0 
            
donttick
            stx    rti_count        ;    the 16bit interrupt count
rtidone     RTI
;***********end of RTI interrupt service routine********

;***********Timer OC6 interrupt service routine***************
oc6isr
            ldd   #3000              ; 125usec with (24MHz/1 clock)
            addd  TC6H               ;    for next interrupt
            std   TC6H               ; 
            bset  TFLG1,%01000000    ; clear timer CH6 interrupt flag, not needed if fast clear enabled
            
            ;;Get analog signal
            LDAA  #%10000111       ; right justified, unsigned, single conversion,
            STAA  ATDCTL5          ; single channel, CHANNEL 7, start the conversion
            
adcwait     ldaa  ATDSTAT0         ; Wait until ATD conversion finish
            anda  #%10000000       ; check SCF bit, wait for ATD conversion to finish
            beq   adcwait
            
            ldaa  ATDDR0L          ; for SIMULATOR, pick up the lower 8bit result
            staa  analogsample
            
            ldx   wavecount
            inx                      ; update OC6 (125usec) interrupt counter
            stx   wavecount 

oc2done     RTI
;***********end of Timer OC6 interrupt service routine********

;***************StartTimer6oc************************
;* Program: Start the timer interrupt, timer channel 6 output compare
;* Input:   Constants - channel 6 output compare, 125usec at 24MHz
;* Output:  None, only the timer interrupt
;* Registers modified: D used and CCR modified
;* Algorithm:
;             initialize TIOS, TIE, TSCR1, TSCR2, TC2H, and TFLG1
;**********************************************
StartTimer6oc
            PSHD
            LDAA   #%01000000
            STAA   TIOS              ; set CH6 Output Compare
            STAA   TIE               ; set CH6 interrupt Enable
            LDAA   #%10000000        ; enable timer, Fast Flag Clear not set
            STAA   TSCR1
            LDAA   #%00000000        ; TOI Off, TCRE Off, TCLK = BCLK/1
            STAA   TSCR2             ;   not needed if started from reset

            LDD    #3000            ; 125usec with (24MHz/1 clock)
            ADDD   TCNTH            ;    for first interrupt
            STD    TC6H             ; 

            BSET   TFLG1,%01000000   ; initial Timer CH6 interrupt flag Clear, not needed if fast clear set
            LDAA   #%01000000
            STAA   TIE               ; set CH6 interrupt Enable
            PULD
            RTS
;***************end of StartTimer2oc*****************

;***************clocktick**********************
;* Program: toggle LED if 1 second is up
;* Input:   rti_count and timermult variables
;* Output:  rti_count variable and 7 segment displays (timer)
;* Registers modified: CCR
;* Algorithm:
;    Check for 1 second passed
;      if not 1 second yet, just pass
;      if 1 second has reached, then increment the timer
;**********************************************
clocktick   ;pshd
            ;pshx                  ; push/pull not needed in RTI

            ldaa   time             ; load value displayed on the timer
            inca                    ;     inc timer
            
            tab
            andb   #$0F             
            cmpb   #$0A             ; check if timer has reached [X A]
            bne    dontresetA       ;     (avoiding hex values) 
            adda   #6               ; add 6 so [X A] => [X+1 0] 
dontresetA  
            cmpa   #$60             ; check if timer has reached 60
            bne    dontresetB
            ldaa   #0               ; if so, reset to 0
dontresetB   
            staa   PORTB            ; send new timer value to display
            staa   time             ; save new timer value in memory
            
            ;pulx
            ;puld
            rts
;***************end of LEDtoggle***************

;***********reset******************************
;* Program: Reset the typespace, printspace,  
;*          numspace, and mathspace as the program  
;*          relies on these chars to act properly
;* Input:   None
;* Output:  typespace and printspace set to $00
;*          numspace and mathspace set to $FF
;* 
;* Registers modified: CCR, Y, A, X
;* Algorithm:
;     Cycle through typespace, printspace, and numspace
;     setting each byte to $00. Print 'Tcalc>' to prepare
;     for the next command
;**********************************************
reset                                
           ldy   #typespace          
           ldaa  #$00
rstloopA    
           staa  1,Y+
           cpy   #numspace         ; loop until numspace , storing $00
           bne   rstloopA

           ldaa  #$FF
rstloopB    
           staa  1,Y+
           cpy   #endspace         ; loop until endspace , storing $FF
           bne   rstloopB
           
           
           ldy   #typespace        ; reset Y to input pointer    
           ldx   #cmdmsg           ; Prints 'Ecalc>' for next input
           jsr   printmsg
           rts 
;****************end of reset******************

;***********validate***************************
;* Program: Validates input and jumps to subroutine
;*          that executes the operation
;* Input:   Register Y points to ASCII characters in memory.
;* Output:  None
;* 
;* Registers modified: CCR, X, Y, D
;* Algorithm:
;     Check for 3 or less intergers, setting a byte to that value
;     Convert those bytes into hex values through numfix subroutine
;     Check for an operation sign
;     Repeat first two steps for second operator of 3 or less intergers
;     Detect input errors, print input up until error and warn user
;**********************************************
validate     
            
            ldx   #numspace
            ldaa  Y
            
            cmpa  #'Q'      ; check for quit command
            beq   quit     
            cmpa  #'q'
            beq   quit
            
            cmpa  #'S'      ; check for set timer command
            beq   setclk
            cmpa  #'s'
            beq   setclk
            
            cmpa  #'A'      ; check for generate wave command
            beq   analogsig
            cmpa  #'a'
            beq   analogsig  
              
bad_input                    ; print invalid command message
            ldaa  #CR        ; move to next line
            jsr   putchar
            ldx   #badinputmsg       
            jsr   printline  ; print error message
            jsr   reset      ; reset space and print prompt
            rts
            
quit
            ldaa  1,+Y
            cmpa  #$00       ; make sure quit command was just q
            bne   bad_input
            
            ldaa  #CR        ; move to next line
            jsr   putchar
            ldx   #msg1      ; print typewriter intro messages
            jsr   printline
            ldx   #msg2
            jsr   printline
            
            pulx                ; pull return address
            lbra  typewriter
            
setclk
            jsr   setclock
            ldaa  #CR           ; move to next line
            jsr   putchar
            jsr   reset
            rts
            
analogsig
            iny
            ldaa  1,Y+
            
            
            cmpa  #'D'      ; check for rest of the command
            beq   nextchar
            cmpa  #'d'      ; cheking for 'd'
            beq   nextchar
            bra   bad_input ; if none of the above, bad input
nextchar            
            ldaa  1,Y+
            
            cmpa  #'C'      
            beq   validcmd
            cmpa  #'c'      ; checking for 'c'
            beq   validcmd
            bra   bad_input ; if none of the above, bad input
validcmd            
            ldaa  1,Y+
            cmpa  #' '
            beq   safe
            cmpa  #nullchar   ; check for another character
            bne   bad_input   ;     if present, bad input
safe            
            jsr   execute     ; get analog signal
            jsr   reset
            rts            
;****************end of validate***************

;***********setclock***************************
;* Program: Set clock to inputted value
;* Input:   Bytes in numspace
;* Output:  Modified 7 segment displays and time variable
;* 
;* Registers modified: CCR, D, Y, X
;* Algorithm:
;     Check for a space after the known 's' input
;     Check for proper 1 or 2 decimal digit input
;     Modify input in necessary
;     Send input to display and store in memory
;     Print error message if any step fails
;**********************************************
setclock    
            iny
            ldaa  1,Y+
            cmpa  #' '      ; make sure there is a space after s command
            bne   bad_set
            
            ldab  #2
setnumchk     
            ldaa  1,Y+
            jsr   numcheck  ; fix numcheck to only validate decimal numbers
           
            cmpa  #$FF      ; numcheck will return $FF if char isn't a number or sign
            beq   bad_set
            
            cmpa  #$10      ; check for nullchar for 1 digit input
            beq   onedigset ;   (numcheck will modify null char to 10) 
                       
            cmpa  #$0A      ; numcheck will not modify signs/CR/space, so they will be greater then A
            bhi   bad_set   ;   (this is for inputs less then 3 digits)
                       
            staa  1,X+      ; store numbers in numspace
           
            decb
            bne   setnumchk
            
            ldaa  1,Y+
            jsr   numcheck  ; checks for a 3rd char in operator
            cmpa  #$10
            bne   bad_set   ; if 3rd char, bad input
            
            ldd   numspace
            lsla
            lsla
            lsla
            lsla            ; shift over 10s digit
            aba             ; add 10s digit and 1s digit
            cmpa  #$5A
            bhi   bad_set   ; checks for a 2 digit number above 60
            staa  time      ; store time value in memory
            staa  PORTB     ; send time value to 7 segment
            rts           
onedigset                   
            ldaa  numspace  ; if input in only one digit,
            staa  time      ;    no modification is needed
            staa  PORTB
            rts
bad_set
            ldaa  #CR
            jsr   putchar
            ldx   #badsetmsg   ; print bad_set message        
            jsr   printline
            rts                                 
;****************end of setclock***************

;***********execute****************************
;* Program: Execute analog signaln acquisition
;* Input:   Bytes in numspace
;* Output:  Prints statement to terminal
;* 
;* Registers modified: CCR, D, Y, X
;* Algorithm:
;     Wait for user to open output file
;     Turn on timer int.
;     Convert each sample into ascii for printing
;     Print result/error number to terminal
;**********************************************
execute   
          jsr     nextline
          jsr     nextline
          jsr     nextline

          ldx     #msg3            ; print '> Set Terminal save file example.txt'
          jsr     printline

          ldx     #msg4            ; print '> Press Enter/Return key to start wave aquisition'
          jsr     printline

mloop2
          jsr     getchar
          cmpa    #0
          beq     mloop2
          cmpa    #CR
          bne     mloop2           ; if Enter/Return key is pressed, move the

          jsr     nextline
          jsr     nextline
          jsr     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last character
          
          
          ldx     #0               ; Enter/Return key hit
          stx     wavecount
          jsr     StartTimer6oc
          ldab    #0
          
nextloop          
          ldd     wavecount     ; load d with currnent wavecount in memeory
          cpd     wavelength    
          bhi     wavedone      ; if count == length, wave is done
waitloop
          cpd     wavecount     ; compare d with wavecount, if not equal, timer
          beq     waitloop      ;    interrupt has incremented counter
          
          ldaa    #0
          ldab    analogsample
          jsr     pnum10                              
              
          bra     nextloop
                        
wavedone            
          ldaa    #%00000000
          staa    TIE
          
          jsr     nextline
          jsr     nextline

          ldx     #msg5            ; print '> Done!  Close Output file.'
          jsr     printline

          ldx     #msg6            ; print '> Ready for next data transmission'
          jsr     printline
          
          rts
;****************end of execute****************

;***********numcheck***************************
;* Program: Change LED state based off of command.
;* Input:   Register A points to ASCII characters in memory.
;* Output:  Change in LED state
;* 
;* Registers modified: CCR, A
;* Algorithm:
;     Check if char in a is a sign ascii
;        if so, pass back unmodified sign
;     Check if char in a is a decimal digit ascii
;        if so, pass back literal digit value
;     If neither, pass back error marker '$FF'
;**********************************************
numcheck
                           ; check for sign
          cmpa  #$2B       ; '+'     
          beq   sign
          cmpa  #$2D       ; '-'     
          beq   sign
          cmpa  #$2A       ; '*'     
          beq   sign
          cmpa  #$2F       ; '/'     
          beq   sign
          cmpa  #$00
          beq   null
          
          suba  #$30      ; checks for 0-9
          cmpa  #$0A
          blo   numpass
          

          ldaa  #$FF      ; sends back FF if number is invalid
          rts
          
numpass   rts             ; send back number in its own byte

null      adda  #$10       ; so the loop catches a nullchar
sign      rts

          
;****************end of numcheck***************

;***********numfix*****************************
;* Program: Reorders bytes in numspace to proper order
;* Input:   Number in B for degree of reordering
;* Output:  Reordeing of numspace
;* 
;* Registers modified: CCR, D
;* Algorithm:
;     Swap last two digits in numspace, then first two
;     Chcck b, if b == 1, need to swap last two again
;     EX b = 2, numspace = $ 01 02 00
;       First two swaps => $ 00 01 02
;       Next swap not needed
;     EX b = 2, numspace = $ 01 00 00
;       First two swaps => $ 00 01 00
;       Next swap       => $ 00 00 01
;**********************************************
numfix       
             pshb
             ldx  #numspace   ; load numspace
             inx
             
             ldd  X           ; load D with 2nd and 3rd numspace bytes
             exg  A,B         ; swap bytes
             std  X
             
             dex
             
             ldd  X           ; load D with 1st and 2nd numspace bytes
             exg  A,B         ; swap bytes
             std  X
             
             pulb
             cmpb #$01        ; if input was two digits, skip last swap
             beq  return
             
             inx
             ldd  X           ; load D with 2nd and 3rd numspace bytes
             exg  A,B         ; swap bytes
             std  X
             
return
             rts
             
;****************end of numfix*****************

;***********pnum10***************************
;* Program: print a word (16bit) in decimal to SCI port
;* Input:   Register D contains a 16 bit number to print in decimal number
;* Output:  decimal number printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Keep divide number by 10 and keep the remainders
;     Then send it out to SCI port
;  Need memory location for counter CTR and buffer BUF(6 byte max)
;**********************************************
pnum10          pshd                   ;Save registers
                pshx
                pshy
                clr     CTR            ; clear character count of an 8 bit number

                ldy     #BUF
pnum10p1        ldx     #10
                idiv
                beq     pnum10p2
                stab    1,y+
                inc     CTR
                tfr     x,d
                bra     pnum10p1

pnum10p2        stab    1,y+
                inc     CTR                        
;--------------------------------------

pnum10p3        ldaa    #$30                
                adda    1,-y
                jsr     putchar
                dec     CTR
                bne     pnum10p3
                jsr     nextline
                puly
                pulx
                puld
                rts
;***********end of pnum10********************

;***********ascii2num**************************
;* Program: Combines bytes in numspace into hex value
;* Input:   Bytes in numspace
;* Output:  16-bit hex value in
;* 
;* Registers modified: CCR, X, Y, D
;* Algorithm:
;     Multiply the first digit by 100 and store the result
;     Multiply the seconf digit by 10 and add to the total
;     Add the 3rd digit to the total and store in numspace
;     Repeat for second operator
;**********************************************
ascii2num    
             ldx    #numspace
next_num             
             ldaa   X             
             ldab   #100
             mul                  ; multiply 1st digit by 100
             std    mathspace     ; store result
             
             
             ldaa   1,X
             ldab   #10
             mul                  ; multiply 2nd digit by 10
             addd   mathspace     ; add to total
             std    mathspace     ; store total
             
             ldab   2,X           ; load 3rd digit
             ldaa   #0   
             addd   mathspace     ; add to total
             std    1,X           ; store total
             
             inx
             inx
             inx
             cpx    #mathspace    ; if x != #mathspace, x holds the sign
             beq    done          ;    otherwise rts
             
             inx                  ; inx to get to the other operator
             bra    next_num      ;    need to loop back around for second number
done             
             rts
;****************end of ascii2num**************   

;***********num2ascii**************************
;* Program: Turns the operation result in mathspace 
;*          into ASCII in printspace
;* Input:   Number in mathspace
;* Output:  ASCII chars in print space
;* 
;* Registers modified: CCR, X, Y, D
;* Algorithm:
;     Load D with result in mathspace
;     Divide by 10,000
;     Store remainder in mathspace for next operation
;     Convert result in X to ASCII (add 30) and store
;        in printspace
;     Repeat first 4 steps with 1000,100,10
;     Store final digit as ascii
;     Remove and 0's before the first digit        
;**********************************************
num2ascii                
            ;ldy   #printspace
            
            ldd   mathspace     ; load D with numspace
            ldx   #10000
            idiv                ; isolate 10,000s digit in X
            std   mathspace     ; save remainder
            tfr   X,D
            addb  #$30          
            stab  1,Y+          ; store 10,000s digit as ascii 
            
            ldd   mathspace     ; repeat for 1000
            ldx   #1000
            idiv  
            std   mathspace
            tfr   X,D
            addb  #$30
            stab  1,Y+

            ldd   mathspace     ; repeat for 100
            ldx   #100
            idiv
            std   mathspace
            tfr   X,D
            addb  #$30
            stab  1,Y+

            ldd   mathspace     ; repeat for 10
            ldx   #10
            idiv
            std   mathspace
            tfr   X,D
            addb  #$30
            stab  1,Y+
                            
            ldd   mathspace     ; store 1s digit 
            addb  #$30
            stab  1,Y+

            ldab  #4
            ;ldy   #printspace
remove0s                        ; remove all the 0s before the first digit
            ldaa  Y             ;    EX 0030 => 30 (turns 0's into null)
            
            cmpa  #$30
            bne   exitremove
            
            suba  #$30
            staa  Y
            iny
            decb
            bne   remove0s                        
exitremove            
            rts
;****************end of num2ascii**************   

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

;****************nextline**********************
nextline
            psha
            ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            pula
            rts
;****************end of nextline***************

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

;****************delay1ms**********************
delay1ms:   pshx
            ldx   #$1000           ; count down X, $8FFF may be more than 10ms 
d1msloop    nop                    ;   X <= X - 1
            dex                    ; simple loop
            bne   d1msloop
            pulx
            rts
;****************end of delay1ms**************** 


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

cmdmsg         DC.B    'HW10> ', $00
spacemsg       DC.B    '       ', $00
badinputmsg    DC.B    'Error: Invalid input', $00
overflowmsg    DC.B    'Error: Overflow', $00
badsetmsg      DC.B    'Error: Invalid time format (only 0 to 59)', $00


menu1          DC.B    'Welcome to the 60-sec timer and analog signal acquisition program', $00
menu2          DC.B    'Type "s XX" to set the clock to any number under 60', $00
menu3          DC.B    'Type "adc" to gather 1024 samples of the incoming signal @ 8 kHz', $00
menu4          DC.B    'Type "q" to exit to the typewriter program', $00
menu5          DC.B    ' ', $00

msg3        DC.B   '> Be sure to start saving Terminal data: open Output file = example.txt', $00
msg4        DC.B   '> When ready, hit Enter/Return key for the selected wave, 1024 point print.', $00

msg5        DC.B   '> Done!  You may close the Output file.', $00
msg6        DC.B   '> Ready for next data command.', $00                       


               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled
