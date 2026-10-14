# qrisc32 Shared Test Vectors

These hex files are the reference programs used by every implemented
qrisc32 language version.  Words are 32-bit instructions, one word per line,
loaded at instruction memory word address zero.

`vectors.json` is the machine-readable manifest for these programs.  Each entry
names the vector, the hex file to load, any optional data-memory init file, the
expected `dmem` words, and the expected PASS line. New implementations should
read this manifest directly.

`toolchain_programs.json` defines generated assembler and tiny-C cases. The
generator writes their instruction/data images to ignored `build/toolchain/`;
`make binary-compat` loads those same bytes on every RTL implementation and checks the
listed memory words after terminal completion. `asm_hazards` assembles this
directory's `hazards.s`; `asm_control_flush` checks two taken-branch wrong-path
stores and a not-taken branch. `c_signed` checks signed comparisons, arithmetic
right shifts, loop state, break/continue, and arrays.

The same manifest expands each tiny-C sort into reversed, mixed, and sorted
data-memory cases. The compiler produces one instruction image per algorithm;
the generator produces case-specific data images and derives expected results
by sorting the ten input integers. `make sort-binary-compat` runs all nine cases
on every RTL implementation.

## smoke.hex

Program: a compact bubble-sort binary generated from
`examples/c/bubble_sort.c` by `qrisc32-cc`.

Initial data memory comes from `smoke_reversed.hex`:

```text
10, 9, 8, 7, 6, 5, 4, 3, 2, 1
```

Expected result: `dmem[0..9] = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10`.

Normal toolchain generation verifies this tracked image byte-for-byte against
fresh compact compiler output. `make update-smoke-golden` is the explicit path
for an intentional, reviewed compiler-output update. The historical optimized
loaders and their broken hand-encoded hex files have been removed; their cycle
figures are archival and are not acceptance criteria for the generated sorts.
Quick sort is not included in the current sort tests.

## axi_smoke.hex

Program:

1. Load `r1 = 5`.
2. Load `r2 = 7`.
3. Add `r3 = r1 + r2`.
4. Store `r3` to `dmem[0]`.
5. Load `dmem[0]` into `r4`.
6. Jump to the terminal loop at byte address `64`.

Expected result: `dmem[0] = 12`.

The AXI protocol benches use this small program and expect
`PASS: AXI smoke dmem[0] = 12` after exercising delayed single-beat AXI
instruction reads, data reads, and data writes.

## flags.hex

Program:

1. Load `r1 = 5`.
2. Load `r2 = 7`.
3. Compare `r1` and `r2`, setting carry for `r1 < r2`.
4. Use `LDRC` and `LDRNC` to select `5` and `7`.
5. Store those values to `dmem[0]` and `dmem[1]`.
6. Jump to the terminal loop at byte address `72`.

Expected result: `dmem[0] = 5`, `dmem[1] = 7`.

Each program includes one trailing NOP after the terminal jump.  It is not part
of the architectural result, but it keeps ports with an already-issued
sequential fetch from reading outside the shared vector file.

## isa.hex

This directed program exercises ISA behavior not covered by the shorter
smoke programs:

1. ADD, SHL, SHR, and MUL results with carry observed through `LDRC/LDRNC`.
2. CMP zero and carry behavior observed through `LDRZ/LDRNZ/LDRC/LDRNC`.
3. JMPZ/JMPNZ/JMPC/JMPNC taken and not-taken paths.

Expected result: `dmem[0..27]` must equal:

```text
0, 1, 0, 0, 1, 0, 0, 1,
0, 0, 1, 0, 1, 0, 0, 1,
0, 1, 1, 0, 0, 1, 1, 0,
1, 0, 0, 1
```

The conditional-jump section checks the no-visible-delay-slot branch contract:
taken branches squash younger side effects, and not-taken branches allow the
fall-through store.

Regression note: the SV reference leaves the observed `LDRC/LDRNC` carry result
clear for the directed SHL and MUL cases in this vector.  The BSV EX stage is
kept aligned to that observed architectural behavior.

## dmem_zero.hex

Initial data-memory contents for the checked `isa.hex` result window.  BSV
RegFile locations are otherwise unspecified in Bluesim, so the ISA testbench
loads these zeros before checking branch-skipped stores against the SV
reference.

## hazards.hex

Program: no-NOP directed dependency coverage for the hazard-aware pipeline
contract.  The source assembly is kept in `hazards.s`.

The vector checks adjacent ALU RAW dependencies, `LDRH/LDRL` use, load-use,
ALU/load-to-store data, base-register address dependencies, CMP-to-LDRF,
CMP-to-conditional-branch-not-taken, CMP-to-conditional-branch-taken squash,
SRC2 auto-increment forwarding, and taken-branch squash through an
unconditional jump.

Expected result: `dmem[0..12]` must equal:

```text
19, 24, 24, 3, 31, 0, 42, 11, 100, 19, 5, 0, 77
```
