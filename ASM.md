# Registers

PC				Program counter
IR				Instruction register
SP				Stack pointer
BP				Base pointer
RR				Return register
C0, C1, S0...	General purpose registers
SR				Status register with flags :
	0				Zero flag : result is 0
	1				Sign flag : result is negative
	2				Overflow flag : overflow (unsigned integers)
	3				Carry flag : borrow (unsigned integers)
	4				Auxiliary carry flag : carry or borrow from lower to upper nibble
    5				Division by zero
    6				Invalid memory access
    7				Stack overflow
    8				Stack underflow

# Instructions

NOP _ _				No op
HALT RX	_			Stop execution with an exit value from RX
SYS_CALL N _		Invoke system call with id N taking parameters from the stack
CLEAR RX _			Load the 0 constant into RX
CONST RX N			Load a constant into RX
CPY RX RY			Copy RY into RX
LOAD RX ADDR		Load a value from memory into RX
STORE RX ADDR		Store a value from RX to memory
SWAP RX RY			Swap the values of RX and RY
CMP RX RY			Compare RX and RY
NEG RX _			Negate RX
INC RX _			Increment RX by 1
DEC RX _			Decrement RX by 1
ADDC RX N			Add RX and a constant
SUBC RX N			Subtract a constant from RX
MULC RX N			Multiply RX and a constant
DIVC RX N			Divide RX with a constant
ADD RX RY			Add RX and RY
SUB RX RY			Subtract RY from RX
MUL RX RY			Multiply RX and RY
DIV RX RY			Divide RX with RY
NOT RX _			Perform a bitwise NOT operation in RX
AND RX RY			Perform a bitwise AND operation between RX and RY
OR RX RY			Perform a bitwise OR operation between RX and RY
XOR RX RY			Perform a bitwise XOR operation between RX and RY
NAND RX RY			Perform a bitwise NAND operation between RX and RY
NOR RX RY			Perform a bitwise NOR operation between RX and RY
NXOR RX RY			Perform a bitwise NXOR operation between RX and RY
SHL RX RY			Shift the bits in register RX left by RY positions
SHR RX RY			Shift the bits in register RX right by RY positions
RCL RX RY			Rotate the bits in RX left by RY positions with carry
RCR RX RY			Rotate the bits in RX right by RY positions with carry
BSWAP RX RY			Swap the byte order of the value in RX
PUSHC N _			Push a constant onto the stack
PUSH RX _			Push the value of RX onto the stack
POP RX _			Pop the top value from the stack and discard it
DROP _ _			Pop the top value from the stack into RX
DUP RX _			Duplicate the value in RX and push it onto the stack
CALL ADDR _			Call a subroutine at the specified address
RET _ _				Return the value from the subroutine
JMP _ ADDR			Jump to a specified address
JEQ RX ADDR			Jump if RX is equal to zero
JNE RX ADDR			Jump if RX is not equal to zero
JGT RX ADDR			Jump if RX is greater than zero
JGE RX ADDR			Jump if RX is greater than or equal to zero
JLT RX ADDR			Jump if RX is lesser than zero
JLE RX ADDR			Jump if RX is lesser than or equal to zero
PRINT RX _			Print the value of RX to stdout
EPRINT RX _			Print the value of RX to stderr
DUMP _ _			Print the value of all registers to strerr

