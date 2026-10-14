# qrisc32 in Bluespec Classic

This directory contains five-stage Bluespec Classic RTL. The shared smoke,
flags, ISA, hazards, and AXI tests run under Bluesim. The earlier architectural
model remains available under `model-*` targets.

Set `BSC_HOME` to the Bluespec installation prefix (containing `bin/bsc` and
`lib/`) and `PICCOLO_ROOT` to the Piccolo source checkout before running local
targets. `BSC`, `DISTRO`, and `BSV_LIBS` remain available as direct overrides.

Keep this directory independent from physical-design flows. The SystemVerilog
implementation defines the qrisc32 behavior. Bluespec Classic consumes the
shared programs in `../tests/` through padded build-local hex images.
The RTL stages instruction fetch, decode, execute, memory response handling,
and writeback explicitly; architectural register and flag commits happen in WB.
Single-entry FIFOs separate IF, ID, EX, MEM, and WB so different instructions
can be in flight together. Dependency masks interlock register and flag uses,
memory commands wait for AXI responses, and taken branches flush younger fetches.

Source layout:

- `rtl/`: Bluespec Classic (`.bs`) qrisc32 RTL with AXI instruction/data masters.
- `src/`: preserved Bluespec Classic (`.bs`) architectural model.
- `tb/`: Bluespec Classic (`.bs`) Bluesim RTL and model testbenches.
- `project/`: legacy/generated project metadata.
- `build/`: generated compiler, Verilog, simulator, VCD, and synthesis outputs only.

Local checks:

```sh
make -C BSC smoke flags isa axi hazards
make -C BSC model
make -C BSC verilog synth
make -C BSC lint
```

Each Bluesim test writes a waveform under `build/vcd/`.

`make lint` compiles the Bluespec Classic RTL benches with stricter BSC warning
flags and runs Verilator lint on generated BSC HDL with scoped waivers for
generated scheduling and AXI xactor internals.
