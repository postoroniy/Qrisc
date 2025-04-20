# qrisc32 in Amaranth

Status: pipelined Amaranth RTL plus a pytest architectural model. The generated
Verilog is checked against the shared flags, ISA, and AXI smoke benches, and the
Amaranth smoke target uses the same original sort bench as SystemVerilog.

The SystemVerilog implementation is the canonical qrisc32 behavior.  Amaranth
work consumes `../tests/vectors.json` and reproduces the same architectural
results with generated Verilog checked by the existing SystemVerilog Verilator
benches. The Python model remains as a pytest diagnostic reference.

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
make model
```
