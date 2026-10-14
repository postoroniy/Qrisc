# qrisc32 in Amaranth

This directory contains five-stage Amaranth RTL and a pytest architectural
model. The generated Verilog runs the shared flags, ISA, AXI, hazards, and sort
tests.

The SystemVerilog implementation defines the qrisc32 behavior. Amaranth
consumes `../tests/vectors.json` and reproduces the same architectural
results with generated Verilog checked by the existing SystemVerilog Verilator
benches. The RTL has explicit IF, ID, EX, MEM, and WB stage state, with RF
commits driven from registered WB sources. The Python model provides a separate
pytest check. The decoder uses dependency-specific stalls and EX/MEM/WB
forwarding rather than a fixed post-instruction bubble.

Source layout:

- `src/`: Amaranth RTL plus the Python architectural model package.
- `tb/`: pytest harnesses for model diagnostics.
- `build/`: generated outputs only.

Local checks from the repository root:

```sh
make amaranth-venv
make amaranth
```

Equivalent local checks from this directory:

```sh
make venv
make smoke flags isa axi hazards
make test
make verilog
make synth
make model
make lint
```

`make lint` runs Python syntax checks, pytest vector diagnostics, and Verilator
lint on generated Amaranth HDL with scoped generated-code waivers.
