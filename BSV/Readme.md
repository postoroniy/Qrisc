# qrisc32 in Bluespec SystemVerilog

This directory contains the five-stage Bluespec SystemVerilog implementation.

Set `BSC_HOME` to the Bluespec installation prefix (containing `bin/bsc` and
`lib/`) and `PICCOLO_ROOT` to the Piccolo source checkout before running local
targets. `BSC`, `DISTRO`, and `BSV_LIBS` remain available as direct overrides.
No host-specific checkout path is built into the Makefile or Yosys flow.

The BSV implementation is checked against SystemVerilog with the shared test
vectors in `../tests/`.  The core is organized as explicit IF, ID, EX, MEM, and
WB stages.  The register file is a standalone RTL block read by ID and committed
from WB. Decode uses a dependency scoreboard to wait for outstanding register,
load, flag, and branch effects; independent instructions can overlap.

Source layout:

- `rtl/`: qrisc32 BSV implementation packages.
- `tb/`: executable BSV testbenches and AXI memory models.
- `build/`: generated BSC, Bluesim, Verilog, VCD, log, and Yosys outputs.
- `libs/`: standalone BSV experiments and examples, excluded from qrisc32 tests.

```sh
make -C BSV smoke
make -C BSV flags
make -C BSV isa
make -C BSV axi
make -C BSV hazards
```

Both testbenches load the same hex programs as the SystemVerilog testbenches and
fail the process on mismatched architectural results.

Generated Bluespec, Bluesim, Verilog, and Yosys outputs are kept under
`build/`.  The BSV source root should contain only source files, docs, scripts,
and project subdirectories.

The current IF and MEM stages use simple single-beat AXI-style transactions.
Burst fetch/read buffering can be added after the `smoke`, `flags`, `isa`,
`hazards`, `axi`, and `tools/equiv_*.sh` tests continue to pass.

Opt-in lint:

```sh
make -C BSV lint
```

This compiles BSV sources with stricter BSC warning flags and runs Verilator
lint on generated HDL with scoped waivers for BSC/FIFO generated internals.
