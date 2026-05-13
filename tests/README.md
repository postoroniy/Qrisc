# qrisc32 Shared Test Vectors

These hex files are the canonical smoke programs used by every implemented
qrisc32 language version.  Words are 32-bit instructions, one word per line,
loaded at instruction memory word address zero.

`vectors.json` is the machine-readable manifest for these programs.  Each entry
names the vector, the hex file to load, any optional data-memory init file, the
expected `dmem` words, and the expected PASS line.  New language ports should use
that manifest as their source of truth.

## smoke.hex

Program: the original optimized qrisc32 bubble-sort smoke program from
`SystemVerilog/tb/Qrisc32Tb.sv`.

Initial data memory comes from `smoke_reversed.hex`:

```text
10, 9, 8, 7, 6, 5, 4, 3, 2, 1
```

Expected result: `dmem[0..9] = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10`.

The original SystemVerilog smoke bench also runs the historical modified-shell
and first-shell sort loaders from `Qrisc32Tb.sv` against reverse, random, and
already-sorted input data.  The generated `smoke_shell_mdf.hex` and
`smoke_shell_first.hex` files are kept for that compatibility work, but the
portable manifest smoke vector uses the bubble-sort binary.  There is no
quick-sort loader in the current `Qrisc32Tb.sv` source.

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

This directed parity program exercises ISA behavior not covered by the shorter
smoke programs:

1. ADD, SHL, SHR, and MUL results with carry observed through `LDRC/LDRNC`.
2. CMP zero and carry behavior observed through `LDRZ/LDRNZ/LDRC/LDRNC`.
3. JMPZ/JMPNZ/JMPC/JMPNC taken and not-taken paths.

Expected result: `dmem[0..27]` must equal:

```text
0, 1, 0, 0, 0, 1, 0, 1,
0, 0, 0, 1, 1, 0, 0, 1,
0, 1, 1, 0, 0, 1, 1, 0,
1, 0, 0, 1
```

The conditional-jump section checks whether a branch skips a delayed store.
Expected zero values in that section mean the corresponding branch was taken.

Regression note: the SV reference leaves the observed `LDRC/LDRNC` carry result
clear for the directed SHL and MUL cases in this vector.  The BSV EX stage is
kept aligned to that observed architectural behavior.

## dmem_zero.hex

Initial data-memory contents for the checked `isa.hex` result window.  BSV
RegFile locations are otherwise unspecified in Bluesim, so the ISA testbench
loads these zeros before checking branch-skipped stores against the SV
reference.
