# qrisc32 in Bluespec SystemVerilog

Status: implemented parity target.

The BSV port is aligned to the SystemVerilog reference through the shared test
vectors in `../tests/`.  The core is organized as explicit IF, ID, EX, MEM, and
WB stages.  The register file is a standalone RTL block read by ID and committed
from WB, with WB forwarding feeding decode for binary compatibility.

Source layout:

- `rtl/`: qrisc32 BSV implementation packages.
- `tb/`: executable BSV testbenches and AXI memory models.
- `build/`: generated BSC, Bluesim, Verilog, VCD, log, and Yosys outputs.
- `libs/`: standalone BSV experiments/examples, not part of the qrisc32 parity gate.

```sh
make -C BSV smoke
make -C BSV flags
make -C BSV isa
make -C BSV axi
```

Both testbenches load the same hex programs as the SystemVerilog testbenches and
fail the process on mismatched architectural results.

Generated Bluespec, Bluesim, Verilog, and Yosys outputs are kept under
`build/`.  The BSV source root should contain only source files, docs, scripts,
and project subdirectories.

The current IF and MEM stages use simple single-beat AXI-style transactions.
Burst fetch/read buffering can be reintroduced later, but only behind the shared
`smoke`, `flags`, `isa`, `axi`, and `tools/equiv_*.sh` parity gates.

Opt-in lint:

```sh
make -C BSV lint
```

This compiles BSV sources with stricter BSC warning flags and runs Verilator
lint on generated HDL with scoped waivers for BSC/FIFO generated internals.
