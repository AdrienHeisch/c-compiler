# Registers

- PC				Program counter
- IR				Instruction register
- SP				Stack pointer
- BP				Base pointer
- LR				Link register
- RR				Return register
- R0 .. R3		General purpose caller-saved registers
- R4 .. R7		General purpose callee-saved registers
- SR				Status register with flags
	- ZF				Zero flag : result is 0
	- SF				Sign flag : result is negative
	- OF				Overflow flag : overflow (unsigned integers)
	- CF				Carry flag : borrow (unsigned integers)
	- ACF				Auxiliary carry flag : carry or borrow from lower to upper nibble
    - DZF				Division by zero
    - IMF				Invalid memory access
    - SOF				Stack overflow
    - SUF				Stack underflow
- FR				Used to store a status flag

# Instructions

With N being a register, a constant or a label

| OP   | Instruction | Description                                                   |
| ---- | ----------- | ------------------------------------------------------------- |
| 0x00 | NOP _ _     | No op                                                         |
| 0x01 | HALT _ N    | Stop execution with an exit value N                           |
| 0x02 | SYCALL _ N  | Invoke system call with id N taking parameters from the stack |
| 0x03 | CLEAR RX _  | Load the 0 constant into RX                                   |
| 0x04 | SET RX N    | Load N into RX                                                |
| 0x05 | LOAD RX N   | Load the value at address N into RX                           |
| 0x06 | STRB RX N  | Store N at the address contained in RX                        |
| 0x07 | STRH RX N  | Store N at the address contained in RX                        |
| 0x08 | STRW RX N  | Store N at the address contained in RX                        |
| 0x09 | STRD RX N  | Store N at the address contained in RX                        |
| 0x0A | SWAP RX RY  | Swap the values of RX and RY                                  |
| 0x0B | CMP RX N    | Compare RX with N                                             |
| 0x0C | NEG RX _    | Negate RX                                                     |
| 0x0D | INC RX _    | Increment RX by 1                                             |
| 0x0E | DEC RX _    | Decrement RX by 1                                             |
| 0x0F | ADD RX N    | Add RX and N                                                  |
| 0x10 | SUB RX N    | Subtract N from RX                                            |
| 0x11 | MUL RX N    | Multiply RX and N                                             |
| 0x12 | DIV RX N    | Divide RX with N                                              |
| 0x13 | MOD RX N    | Modulo RX with N                                              |
| 0x14 | NOT RX _    | Perform a bitwise NOT operation in RX                         |
| 0x15 | AND RX N    | Perform a bitwise AND operation between RX and N              |
| 0x16 | OR RX N     | Perform a bitwise OR operation between RX and N               |
| 0x17 | XOR RX N    | Perform a bitwise XOR operation between RX and N              |
| 0x18 | NAND RX N   | Perform a bitwise NAND operation between RX and N             |
| 0x19 | NOR RX N    | Perform a bitwise NOR operation between RX and N              |
| 0x1A | NXOR RX N   | Perform a bitwise NXOR operation between RX and N             |
| 0x1B | SHL RX N    | Shift the bits in register RX left by N positions             |
| 0x1C | SHR RX N    | Shift the bits in register RX right by N positions            |
| 0x1D | RCL RX N    | Rotate the bits in RX left by N positions with carN           |
| 0x1E | RCR RX N    | Rotate the bits in RX right by N positions with carN          |
| 0x1F | BSWAP RX _  | Swap the byte order of the value in RX                        |
| 0x20 | PUSH _ N    | Push N onto the stack                                         |
| 0x21 | DUP _ N     | Duplicate the value at address N and push it onto the stack   |
| 0x22 | POP RX _    | Pop the top value from the stack into RX                      |
| 0x23 | DROP _ _    | Pop the top value from the stack and discard it               |
| 0x24 | CALL _ N    | Call a subroutine at address N                                |
| 0x25 | RET _ N     | Return N from the subroutine                                  |
| 0x26 | JMP _ N     | Jump to instruction N                                         |
| 0x27 | JEQ RX N    | Jump if RX is equal to zero                                   |
| 0x28 | JNE RX N    | Jump if RX is not equal to zero                               |
| 0x29 | JGT RX N    | Jump if RX is greater than zero                               |
| 0x2A | JGE RX N    | Jump if RX is greater than or equal to zero                   |
| 0x2B | JLT RX N    | Jump if RX is lesser than zero                                |
| 0x2C | JLE RX N    | Jump if RX is lesser than or equal to zero                    |
| 0x2D | PRINT _ N   | Print N to stdout                                             |
| 0x2E | EPRINT _ N  | Print N to stderr                                             |
| 0x2F | DUMP _ _    | Print the value of all registers to strerr                    |

# Encoding

- OP CODE			: 1 byte, first bit = right is register
- OP LEFT			: 1 bytes
- OP RIGHT		: 8 bytes
