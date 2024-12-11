# Registers

PC				Program counter
IR				Instruction register
SP				Stack pointer
BP				Base pointer
RR				Return register
PR				Used to store a pointer for operations
C0, C1, S0...	General purpose registers
SR				Status register with flags
	ZF				Zero flag : result is 0
	SF				Sign flag : result is negative
	OF				Overflow flag : overflow (unsigned integers)
	CF				Carry flag : borrow (unsigned integers)
	ACF				Auxiliary carry flag : carry or borrow from lower to upper nibble
    DZF				Division by zero
    IMF				Invalid memory access
    SOF				Stack overflow
    SUF				Stack underflow
FR				Used to store a status flag

# Instructions

With N being a register, a constant or eventually a label

0x00	NOP _ _				No op
0x01	HALT N	_			Stop execution with an exit value N
0x02	SYS_CALL N _		Invoke system call with id N taking parameters from the stack
0x03	CLEAR RX _			Load the 0 constant into RX
0x04	SET RX N			Load N into RX
0x05	LOAD RX N			Load the value at address N into RX
0x06	STORE RX N			Store N at the address contained in RX
0x07	SWAP RX RY			Swap the values of RX and RY
0x08	CMP RX N			Compare RX with N
0x09	NEG RX _			Negate RX
0x0A	INC RX _			Increment RX by 1
0x0B	DEC RX _			Decrement RX by 1
0x0C	ADD RX N			Add RX and N
0x0D	SUB RX N			Subtract N from RX
0x0E	MUL RX N			Multiply RX and N
0x0F	DIV RX N			Divide RX with N
0x10	NOT RX _			Perform a bitwise NOT operation in RX
0x11	AND RX N			Perform a bitwise AND operation between RX and N
0x12	OR RX N				Perform a bitwise OR operation between RX and N
0x13	XOR RX N			Perform a bitwise XOR operation between RX and N
0x14	NAND RX N			Perform a bitwise NAND operation between RX and N
0x15	NOR RX N			Perform a bitwise NOR operation between RX and N
0x16	NXOR RX N			Perform a bitwise NXOR operation between RX and N
0x17	SHL RX N			Shift the bits in register RX left by N positions
0x18	SHR RX N			Shift the bits in register RX right by N positions
0x19	RCL RX N			Rotate the bits in RX left by N positions with carN
0x1A	RCR RX N			Rotate the bits in RX right by N positions with carN
0x1B	BSWAP RX			Swap the byte order of the value in RX
0x1C	PUSH N _			Push N onto the stack
0x1D	DUP N _				Duplicate the value at address N and push it onto the stack
0x1E	POP RX _			Pop the top value from the stack into RX
0x1F	DROP _ _			Pop the top value from the stack and discard it
0x20	CALL N _			Call a subroutine at address N
0x21	RET _ _				Return the value from the subroutine
0x22	JMP _ N				Jump to instruction N
0x23	JEQ RX N			Jump if RX is equal to zero
0x24	JNE RX N			Jump if RX is not equal to zero
0x25	JGT RX N			Jump if RX is greater than zero
0x26	JGE RX N			Jump if RX is greater than or equal to zero
0x27	JLT RX N			Jump if RX is lesser than zero
0x28	JLE RX N			Jump if RX is lesser than or equal to zero
0x29	PRINT RX _			Print the value of RX to stdout
0x2A	EPRINT RX _			Print the value of RX to stderr
0x2B	DUMP _ _			Print the value of all registers to strerr

# Encoding

OP CODE  : 1 byte
OP LEFT  : 8 bytes
OP RIGHT : 8 bytes
