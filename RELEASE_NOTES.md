# qrisc32 v0.1.0

This release establishes SystemVerilog as the reference ISA and provides five
additional binary-compatible RTL implementations in BSV, Amaranth, Clash,
Bluespec Classic, and ZLang.

## ZLang port

- Requires the released `zlang-hdl` `0.1.0a23` distribution or newer; the
  release check requires exactly a23 by default for reproducibility.
- Implements overlapping IF, ID, EX, MEM, and WB stages with scoreboard-based
  dependency interlocks and taken-branch squash.
- Uses the native `std.bus.axi4` typed interfaces for single-beat instruction
  reads and data reads/writes, including AXI IDs and USER fields.
- Runs smoke, flags, ISA, hazards, AXI backpressure, generated assembler/C, and
  all nine generated sort cases through the native JIT.
- Emits SystemVerilog for independent Verilator linting and includes source
  assertions, cover goals, and formal entry points.

## Shared toolchain and compatibility

- `qrisc32-as` and `qrisc32-cc` generate the instruction images used by the
  compatibility tests.
- Compact tiny-C output does not insert blanket software NOPs.
- Bubble, modified-shell, and first-shell/insertion sorts are generated from C
  and run unchanged on all six implementations.
- Every generated binary case checks architectural memory, terminal completion,
  and non-empty VCD output.

## Validation

The release command is `make release-check`. It runs simulator tests, lint,
toolchain and binary compatibility tests, generic synthesis, ZLang generated
HDL lint, presentation generation, version checks, and a check for local paths.

## Known limitation

The ZLang AXI top is intentionally single-beat and single-outstanding. Its
directed protocol bench keeps `enable` asserted. Unconstrained formal
deassertion of `enable` while an instruction AR request is backpressured is not
supported; the AXI test keeps `enable` asserted.
