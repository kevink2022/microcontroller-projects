# Microprocessors & Embedded Systems Course

This course focused on learning how to architect and construct highly time and resource-sensitive applications on a microprocessor or embedded system. The projects were written in the **S12CPUV2 Assembly Language** and were run on a simulated chip from the  **MC9S12C Chip Family**. 

The topic included in the course and the projects include:
- Intro to Assembly Language Programming
- Addressing Modes
- Subroutines & Stacks
- Arithmetic Operations
- Branching & Looping
- Interrupts
- Parallel I/O
- Serial Peripheral Interface (SPI)
- Analog/Digital Conversions
- Timers
- Serial Communication Interface (SCI)
- Signals and Busses
- Embedded Processing

There were 10 homework assignments in the semester I took, but they generally built on the previous assignments, so I will be featuring three assignments here.

The assignments are unedited from when I took the class in Fall 2021, however, I do not have any of my notes, so all flow charts and descriptions were written for this page, in July 2024. (I sure hope I wrote some good code, or this will be painful.) Any comments made in retrospection will be prepended by 'NOTE:'.

# HW 5
This was the final in a series of assignments working on a program to control LED lights, adding the ability to type commands in the terminal. The final program had the following abilities:
- Receive characters in ASCII format over an SCI port from the keyboard.
- Send characters in ASCII format over an SCI port to the terminal.
- The program starts in LED CONTROL mode until the user quits and it goes into TYPEWRITER mode.
- LED CONTROL MODE:
	- The list of commands is printed on the program start.
	- Users can type up to 5 characters for a command.
	- If the command matches any valid commands, that command is executed.
Commands are case-insensitive
	- There are 4 LEDs. All LEDs but 2 simply toggle on and off. LED 2 fades on/off over 4 seconds.
		- The fade was controlled by a 400-microsecond loop, which was calculated based on the consistent clock speed and the consistent number of clock cycles each instruction took on this chipset. However, since we were on a simulator, it didn't run in real-time and the numbers were adjusted for the submission.
	- The commands are (# = 1-4):
		- L#: Turn # LED on
		- F#: Turn # LED off
		- QUIT: Quit to typewriter mode
- TYPEWRITER MODE:
	- The user can type any ASCII character. The return key simply moves to the next line.

See hw5asm.svg for the logic flowchart, and see routine descriptions along with the source code at hw5.asm.

# HW 8
This assignment required creating a calculator program to perform arithmetic calculations on ASCII user inputs.
- Each operator could be up to three digits.
- The basic operations were allowed: +, -, \*, \
- Negative numbers and spaces were not supported.
- The program would detect and alert the user if an overflow occurred.
- The typewriter program from HW 5 was included when the user quit.

See routine descriptions along with the source code at hw8.asm.

# HW 10
This assignment required digitally capturing an analog signal, as well as running a timer and displaying its status on a 7-second display.
- The commands were as follows:
	- Type "s XX" where XX number from 0-59 to set the timer, which would count from the entered number down to 0.
	- Type "adc" to gather 1024 samples of the incoming signal @ 8 kHz.
	- Type "q" to exit to the typewriter mode.
- The typewriter program from HW 5 was included when the user quit.

See routine descriptions along with the source code at hw10.asm.
