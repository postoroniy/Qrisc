# qrisc32 in Bluespec Classic

Status: Bluespec Classic RTL passing shared smoke, flags, ISA, and AXI smoke
checks.  The previous architectural model remains available under `model-*`
targets.

Keep this directory independent from physical-design flows.  The SystemVerilog
implementation is the canonical qrisc32 behavior, and this port consumes the
shared programs in `../tests/` through padded build-local hex images.

Source layout:

- `rtl/`: Bluespec Classic (`.bs`) qrisc32 RTL with AXI instruction/data masters.
- `src/`: preserved Bluespec Classic (`.bs`) architectural model oracle.
- `tb/`: Bluespec Classic (`.bs`) Bluesim RTL and model testbenches.
- `project/`: legacy/generated project metadata.
- `build/`: generated compiler, Verilog, simulator, VCD, and synthesis outputs only.

Local checks:

```sh
make -C BSC smoke flags isa axi
make -C BSC model
make -C BSC verilog synth
```

Each Bluesim test writes a waveform under `build/vcd/`.
