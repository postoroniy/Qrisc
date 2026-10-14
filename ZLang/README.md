# qrisc32 in ZLang

`src/Qrisc32.zhl` contains the five-stage in-order core and its typed AXI
adapter. The core has IF, ID, EX, MEM, and WB valid state. Its scoreboard stalls
true register/flag dependencies, and EX-stage redirects squash younger
instructions. Independent instructions can occupy all stages together. ISA
encodings live in `src/isa.zhl`; the EX
multiplier has a ZLang synthesis-intent annotation. Source assertions and
cover goals describe memory exclusivity, pipeline overlap, and
interlocks.

The direct `Qrisc32Axi` top uses ZLang's `std.bus.axi4` typed interfaces:
instruction read and combined data read/write, with one-bit IDs and USER
fields matching the SV port widths. Each request is an aligned, single-beat
INCR transfer with full-word WSTRB. The `axi` target checks independent
AR/AW/W backpressure, delayed responses, payload stability, IDs and sidebands,
and the shared architectural result. The source does not support multi-beat
or multiple-outstanding transactions; generated pin names may differ from SV.
Other vectors use zero-wait host memories through the native Cranelift JIT.
All generated waves are under `build/vcd/`.

ZLang `0.1.0a23` is the tested release. Install the `zlang-hdl==0.1.0a23`
Python distribution, which provides the `zlang` CLI; do not install the
unrelated distribution named `zlang`. Alternatively, set `ZLANG_ROOT` to an
a23 checkout with `.venv/bin/python` and `.venv/bin/zlang`.
`ZLANG_PYTHON` and `ZLANG_BIN` can override either executable. The AXI bench
executes the native plan; `make -C ZLang lint` independently checks emitted
SystemVerilog with Verilator. a22 removed the old Python reference engine;
a23 fixes native simulation of output-owned registers used by this core.

```sh
make -C ZLang check
make -C ZLang test
make -C ZLang verilog
make -C ZLang lint
make -C ZLang formal
make -C ZLang legacy-sorts
```

The `formal` target runs ZLang's source verification on both tops; it may
require external solver tools. A bounded depth-4 check passes the core safety
properties. Formal verification of the AXI top requires `enable` to remain
asserted while an instruction AR request is waiting for `ready`; the AXI test
uses that interface rule. The generated-HDL lint
step formats only ignored build output before Verilator because ZLang can emit
expressions longer than Verilator's line-token limit. Generated HDL, reports,
and waves remain under the ignored `build/` directory.

`legacy-sorts` is retained as a compatibility target name, but now runs the
passing tiny-C-generated bubble, modified-shell, and first-shell/insertion
programs against reversed, mixed, and already-sorted data. Each case consumes
the same generated hex used by the other ports and emits its own VCD.
