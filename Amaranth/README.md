# qrisc32 in Amaranth

Status: five-stage Amaranth RTL plus a pytest architectural model. The generated
Verilog is checked against the shared flags, ISA, and AXI smoke benches, and the
Amaranth smoke target uses the same original sort bench as SystemVerilog.

The SystemVerilog implementation is the canonical qrisc32 behavior.  Amaranth
work consumes `../tests/vectors.json` and reproduces the same architectural
results with generated Verilog checked by the existing SystemVerilog Verilator
benches. The RTL has explicit IF, ID, EX, MEM, and WB stage state, with RF
commits driven from registered WB sources. The Python model remains as a pytest
diagnostic reference.

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
make smoke flags isa axi
make test
make verilog
make synth
make model
make lint
```

`make lint` runs Python syntax checks, pytest vector diagnostics, and Verilator
lint on generated Amaranth HDL with scoped generated-code waivers.
