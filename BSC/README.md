# qrisc32 in Bluespec Classic

Status: five-stage Bluespec Classic RTL passing shared smoke, flags, ISA, and
AXI smoke checks.  The previous architectural model remains available under
`model-*` targets.

Keep this directory independent from physical-design flows.  The SystemVerilog
implementation is the canonical qrisc32 behavior, and this port consumes the
shared programs in `../tests/` through padded build-local hex images.
The RTL stages instruction fetch, decode, execute, memory response handling,
and writeback explicitly; architectural register and flag commits happen in WB.

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
make -C BSC lint
```

Each Bluesim test writes a waveform under `build/vcd/`.

`make lint` compiles the Bluespec Classic RTL benches with stricter BSC warning
flags and runs Verilator lint on generated BSC HDL with scoped waivers for
generated scheduling and AXI xactor internals.
