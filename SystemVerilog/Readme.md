# qrisc32 in SystemVerilog

Status: canonical reference implementation.

The public `qrisc32` top level exposes single-beat AXI4-style instruction and
data master interfaces.  The instruction side is read-only; the data side has
read and write channels.  The internal pipeline behavior remains the canonical
SystemVerilog reference.

Source layout:

- `rtl/`: reusable SystemVerilog RTL and package sources.
- `tb/`: SystemVerilog testbenches, memory models, and include files.
- `cpp/`: Verilator C++ harnesses.
- `build/`: generated Verilator, VCD, and Yosys outputs.

Use Verilator for simulator checks:

```sh
make -C SystemVerilog smoke
make -C SystemVerilog flags
make -C SystemVerilog isa
make -C SystemVerilog axi
```

The smoke, flags, and ISA testbenches load shared programs from `../tests/` and
check the expected data-memory results through AXI memory models.  Other
language ports should match these
results before being treated as implemented.

The `axi` target reuses the smoke program with checked AXI memories that apply
deterministic backpressure, delay read/write responses, and fail on invalid
single-beat AXI field values or unstable stalled payloads.

Optional generic synthesis writes to `build/yosys/`:

```sh
make -C SystemVerilog synth
```
