# qrisc32 in Clash

This directory contains the five-stage Clash RTL implementation. Generated HDL
runs the smoke, flags, ISA, hazards, and AXI tests.

The SystemVerilog implementation defines the qrisc32 behavior. Clash
consumes the shared programs described by `../tests/vectors.json` and reproduces
the same architectural results through generated Verilog and Verilator.  The
RTL now stages instruction fetch, decode, execute, memory response handling, and
writeback explicitly; architectural register and flag commits happen in WB.
Fetch, decode/execute, memory, and writeback slots can hold different
instructions concurrently. Dependency masks stall only conflicting issue;
loads wait for AXI read data and taken branches discard younger fetches.

Source layout:

- `src/`: synthesizable qrisc32 RTL plus the retained architectural model.
- `tb/`: generated-HDL bench generator and model harness.
- `build/`: generated outputs only.

Local checks:

```sh
make -C Clash smoke flags isa axi hazards
make -C Clash verilog
make -C Clash synth
make -C Clash lint
```

Model-only checks:

```sh
make -C Clash model
```

Local Clash compiler:

```sh
make clash-install
make clash-version
```

Set `CLASH_COMPILER_ROOT` to a Clash compiler checkout before running
`make clash-install`. The installed binary goes to `./.local/bin/clash` in
this repository. `CLASH_GHC` defaults to the GHC installation reported by the
configured `clash` executable, and `CLASH_PACKAGE_DB` selects its matching
local Cabal package database when available. Override
`CLASH_BIN_DIR` to change the install location.

The lint target runs GHC `-Wall -fno-code` for the RTL and model sources, then
Verilator lint on generated Clash HDL with scoped generated-code waivers.
