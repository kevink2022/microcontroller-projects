***********************************************************************
*
* Title:          CMPEN 472 Homework 8 Submission
*
* Objective:      (1) To learn interrupts and timming: Real Time Interrupt (RTI). 
*                 (2) Also to learn interrupt based multi-tasking programming. 
*
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Oct. 31st, 2021
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

CRGFLG      EQU         $0037        ; Clock and Reset Generator Flags
CRGINT      EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL      EQU         $003B        ; Real Time Interrupt Control

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
rti_count     DC.W      1            ; Value in memory that is incremented every RTI
                                     ;   to keep track of time for the 60 sec timer
time          DS.B      1            ; Value in memory that represents what the timer
                                     ;   is actually displaying
timermult     DC.W      120          ; Since simulator is very inaccurate, adjusting  
                                     ;   this number adjusts timer rate
typespace     DS.B      7            ; The bytes reserved in memory for the typed commands to 
                                     ;   to be stored (limits input characters to this space)
breakspace    DS.B      2            ; Put 2 nullchar between typespace and printspace for
                                     ;   printing to the terminal
printspace    DS.B      6            ; The bytes reserved in memory for the commands to be 
                                     ;   printed on the terminal to be stored
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
            jsr   clocktick         ; check rti_count and modify timer if needed
            
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
            ldx    rti_count
            inx                     ; every time the RTI occur, increase
            stx    rti_count        ;    the 16bit interrupt count
rtidone     RTI
;***********end of RTI interrupt service routine********

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
clocktick   pshd
            pshx

            ldx    rti_count       ; check for 1 sec
;            cpx    #200             ; 2.5msec * 200 = 0.5 sec
            cpx    timermult        ; 2.5msec * timermult = 1 sec
            blo    donttick         ; NOT yet

            ldx    #0               ; 1sec is up,
            stx    rti_count        ;     clear counter to restart

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
donttick
            pulx
            puld
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
            lbeq   quit     
            cmpa  #'q'
            lbeq   quit
            
            cmpa  #'S'      ; check for set timer command
            lbeq   setclk
            cmpa  #'s'
            lbeq   setclk
            
            
            cmpa  #'-'      ; checks to make sure negative numbers aren't inputted
            beq   neg_input ;   (extra step needed since numcheck see negative sign as minus)
            
            ldab  #3
numchk     
            ldaa  1,Y+
            jsr   numcheck  ; fix numcheck to only validate decimal numbers
           
            cmpa  #$FF      ; numcheck will return $FF if char isn't a number or sign
            beq   bad_input
            
            cmpa  #$0A      ; numcheck will not modify signs/CR/space, so they will be greater then A
            bhi   escape    ;   (this is for inputs less then 3 digits)
                       
            staa  1,X+      ; store numbers in numspace
           
            decb
            bne   numchk
           
            ldaa  1,Y+
            jsr   numcheck  ; checks for a 4th digit in operator
            cmpa  #$0A
            blo   bad_input ; if lower then $0A, it is 4th number and bad input 
escape            
            
            cmpb  #0        ; if b == 0, input is 3 digits and doesn't need correcting
            beq   skipnf
                        
            ldaa  #$00
            staa  1,X+       ; fill rest of numspace with '$00' depending on digits on input 
            
            cmpb  #1
            beq   jumpnf
            
            staa  1,X+       ; fill rest of numspace with '$00' depending on digits on input 
jumpnf            
            jsr   numfix     ; fixes input (1 => $01 00 00 => $00 00 1)
skipnf      
            ldx   #numspace
            ldaa  4,X        ; checks if 4th char in numspace if FF. 
            cmpa  #$FF       ; if FF, need to get sign and second operator
            bne   exe        ; if not FF, it is a sign, second operator had been recieved, move to execute
            
            dey
            ldaa  Y          ; gets sign back into A
            iny
                        
                             ; check for signs
            cmpa  #$2B       ; '+'     
            beq   putsign
            
            cmpa  #$2D       ; '-'     
            beq   putsign
            
            cmpa  #$2A       ; '*'     
            beq   putsign
            
            cmpa  #$2F       ; '/'     
            beq   putsign
            
            bra   bad_input  ; if sign not one of the above, bad input
           
putsign
            ldx   #numspace
            staa  3,X
            
            ldaa  X          ; move the number later into numspace    
            staa  4,X        ;   so the same algorithm can be used for the first number
            ldaa  1,X            
            staa  5,X        ; first 3 chars in numspace store numbers from numcheck
            ldaa  2,X        ;   to use numcheck again, move first input to last 3 numspace chars   
            staa  6,X
            
            lbra   validate   ; use the already written code to store next input number

exe
            jsr   execute     ; execute statement
            jsr   reset       ; reset space and print prompt
            rts
neg_input            
            staa  1,Y+   
bad_input                    ; print invalid command message
            ldaa  #$00
            staa  1,Y+       ; store $00 after error to only print until error
            ldaa  #CR        ; move to next line
            jsr   putchar
            ldx   #spacemsg
            jsr   printmsg   ; print spaces to line up output
            ldx   #typespace 
            jsr   printline  ; print input up until error
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
;* Program: Execute validated statemnt
;* Input:   Bytes in numspace
;* Output:  Prints statement to terminal
;* 
;* Registers modified: CCR, D, Y, X
;* Algorithm:
;     Check the sign in numspace to determine operation
;     Execute operation
;     Check for negative in sub and overflow in mul
;     Convert result into ascii for printing
;     Print result/error number to terminal
;**********************************************
execute      jsr   ascii2num
             
             ldx   #numspace
             ldaa  3,X        ; load a with the sign
             
             cmpa  #$2B       ; '+'     
             beq   exe_add
            
             cmpa  #$2D       ; '-'     
             beq   exe_sub
            
             cmpa  #$2A       ; '*'     
             beq   exe_mul
            
             cmpa  #$2F       ; '/'     
             beq   exe_div
             
exe_add
             ldd   1,X          ; load 2nd operator into memory
             std   mathspace
             ldd   5,X          ; load 1st operator into D
             addd  mathspace    ; add D and memory
             std   mathspace
             bra   exe_done       
exe_sub
             ldd   1,X          ; load 2nd operator into memory
             std   mathspace
             ldd   5,X          ; load 1st operator into D
             subd  mathspace    ; subtract D and memory
             std   mathspace
             bmi   negative     ; if sub result is negative, set up output for neg
             bra   exe_done     
exe_mul
             ldy   1,X          ; load 2nd operator into Y
             ldd   5,X          ; load 1st operator into D
             emul               ; multiply D and Y
             cpy   #$00
             bne   overflow     ; if y != 0, overflow had occured
             std   mathspace
             bra   exe_done
exe_div
             tfr   X,Y
             ldx   1,Y          ; load 2nd operator into X
             ldd   5,Y          ; load 1st operator into D
             idiv               ; divide D by Y
             stx   mathspace
             bra   exe_done
             
negative     
             ldd   #$0000
             subd  mathspace    ; set mathspace back to positive
             std   mathspace
             ldab  #$FF         ; set marker for negative number

exe_done   
             pshb               ; push B to save negative marker
             jsr   num2ascii    ; translate operation result in mathspace
             pulb               ; into ascii in printspace
             
             ldaa  #CR
             jsr   putchar
             ldx   #spacemsg    ; print spaces to line up output statement
             jsr   printmsg
             ldx   #typespace   ; echo validated input
             jsr   printmsg
         
             ldaa  #'='
             jsr   putchar
             
             cmpb  #$FF         ; check negative marker
             bne   skipneg      ; if not set, skip printing '-'
             ldaa  #'-'
             jsr   putchar 
skipneg             
             ldx   #printspace  ; load printspace into x
fixprint                        ; print space might start with nullchars, need to inx past
             ldaa  X
             inx                ; load print space first char into A
             cmpa  #$00         ; increment X until the first char of the output
             beq   fixprint
             dex
             jsr   printline            
             
             rts
overflow     
             ldaa  #CR
             jsr   putchar
             ldx   #spacemsg
             jsr   printmsg
             ldx   #typespace   ; echo input
             jsr   printline
             ldx   #overflowmsg ; print overflow message        
             jsr   printline
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
            ldy   #printspace
            
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
            ldy   #printspace
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
            jsr   printline
            ldx   #menu6
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

cmdmsg         DC.B    'Tcalc> ', $00
spacemsg       DC.B    '       ', $00
badinputmsg    DC.B    'Error: Invalid input', $00
overflowmsg    DC.B    'Error: Overflow', $00
badsetmsg      DC.B    'Error: Invalid time format (only 0 to 59)', $00


menu1          DC.B    'Welcome to the 16-bit Calculator Program!', $00
menu2          DC.B    'Only simple operations allowed (+,-,*,/)', $00
menu3          DC.B    'Input operators limited to 3 digits', $00
menu4          DC.B    'No negative numbers or spaces', $00
menu5          DC.B    'Overflow errors will be caught', $00
menu6          DC.B    ' ', $00                       


               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled
