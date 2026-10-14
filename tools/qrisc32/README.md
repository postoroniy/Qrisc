# qrisc32 Toolchain

This package contains the repo-local qrisc32 assembler and tiny C-subset
compiler.

- `qrisc32-as`: assembles repo-native qrisc32 assembly to instruction hex.
- `qrisc32-cc`: compiles one `int main(void)` tiny C program to assembly and
  optional instruction/data hex.
- `isa.py`: shared opcode, sub-opcode, flag, jump, and register aliases.
- `encoder.py`: bit-exact instruction encoders shared by the assembler and
  compiler.

The assembly syntax follows the ISA mnemonics used by the RTL and docs:
`LDRH`, `LDRL`, `LDRP`, `STRP`, `ADD`, `CMP`, `LDRC`, `JMP`, `JMPZ`, and
related aliases.  The C compiler is intentionally small and has no ABI, libc,
function calls, pointers, structs, or dynamic allocation.

Tiny-C `int` is signed 32-bit two's complement, with wrapping arithmetic.
Relational comparisons use signed ordering and `>>` sign-extends negative
values; shift counts must be 0-31. Array storage maps to data memory. The
compiler emits no dependency padding by default: `--schedule=compact` is the
default, `legacy` inserts six NOPs after instructions for diagnosis, and
`safe` and `none` currently behave like `compact` (no reordering or bubbles).
The terminal-loop guard NOP is intentional.

`tests/toolchain_programs.json` lists the expected results for generated binary
tests. `make binary-compat` assembles and compiles
those sources to ignored `build/toolchain/`, then runs identical images on all
six RTL ports with checked AXI memories where their pin contracts match. The
ZLang run uses its native JIT and its own AXI wrapper.

Bubble, modified-shell, and first-shell/insertion sources live in
`examples/c/`. Their reversed, mixed, and sorted cases are expanded by the
manifest generator without duplicating instruction images. `make sort-test`
runs them on the SV reference and `make sort-binary-compat` runs the same bytes
on every implementation. Normal generation also verifies that compact `bubble_sort.c`
output exactly matches the tracked public `tests/smoke.hex` golden.
