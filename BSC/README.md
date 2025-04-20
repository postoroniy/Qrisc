# qrisc32 in Bluespec Classic

Status: staged Bluespec Classic source model passing shared smoke, flags, ISA,
and AXI smoke checks.

Keep this directory independent from physical-design flows.  The SystemVerilog
implementation is the canonical qrisc32 behavior, and this port consumes the
shared programs in `../tests/` through padded build-local hex images.

Source layout:

- `src/`: staged Bluespec Classic (`.bs`) qrisc32 architectural model.
- `tb/`: Bluespec Classic (`.bs`) Bluesim testbenches for shared vectors.
- `project/`: legacy/generated project metadata.
- `build/`: generated compiler, simulator, and VCD outputs only.

Local checks:

```sh
make -C BSC smoke flags isa axi
```

Each Bluesim test writes a waveform under `build/vcd/`.
