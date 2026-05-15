# qrisc32 Toolchain

This package contains the repo-local qrisc32 assembler and tiny C-subset
compiler.

- `qrisc32-as`: assembles repo-native qrisc32 assembly to instruction hex.
- `qrisc32-cc`: compiles one `int main(void)` tiny C program to assembly and
  optional instruction/data hex.
- `isa.py`: shared opcode, sub-opcode, flag, jump, and register aliases.
- `encoder.py`: bit-exact instruction encoders used by both tools and the
  historical sort-vector generator.

The assembly syntax follows the ISA mnemonics used by the RTL and docs:
`LDRH`, `LDRL`, `LDRP`, `STRP`, `ADD`, `CMP`, `LDRC`, `JMP`, `JMPZ`, and
related aliases.  The C compiler is intentionally small and has no ABI, libc,
function calls, pointers, structs, or dynamic allocation.
