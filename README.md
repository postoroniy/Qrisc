# Qrisc

Qrisc32 is a small 32-bit RISC CPU implemented in several hardware description
languages. The SystemVerilog implementation defines the reference behavior.
Every other implementation runs the same instruction images and must produce
the same architectural results.

Project version: `0.1.0`. See `CHANGELOG.md`, `RELEASE_NOTES.md`, and
`RELEASING.md` for release scope, notes, and reproducible qualification steps.

## Status

| Directory | Status |
| --- | --- |
| `SystemVerilog/` | Reference five-stage implementation with AXI-style instruction/data interfaces; smoke, flags, ISA, and AXI protocol tests pass with Verilator. |
| `BSV/` | Five-stage Bluespec SystemVerilog implementation with AXI instruction/data interfaces; smoke, flags, ISA, and AXI protocol tests pass against the shared vectors. |
| `Amaranth/` | Five-stage Amaranth RTL tested through generated Verilog; the Python model provides a separate functional check. |
| `Clash/` | Five-stage Clash RTL with AXI instruction/data interfaces, tested through generated Verilog and Verilator. |
| `BSC/` | Five-stage Bluespec Classic RTL with AXI instruction/data interfaces and Bluesim tests; the earlier model remains available for comparison. |
| `ZLang/` | Five-stage ZLang RTL with native-JIT architectural vectors and a checked, typed one-beat AXI4 instruction/data interface. |

The old foundry-specific physical-design flow has been removed. The repository
uses simulator tests by default, with optional generic
Yosys checks writing only under ignored build directories.

Each language directory keeps source and generated files separate.  Implemented
ports use `rtl/` or `src/` for implementation sources, `tb/` for simulator
testbenches, and `build/` for generated compiler, simulator, waveform, and
synthesis outputs.

All implemented RTL flows export the same public generated-HDL top module name:
`qrisc32`.  Source-language constructors may keep their idiomatic names, but
Verilator and Yosys targets use `qrisc32` consistently.

ISA opcode, sub-opcode, flag, jump, and increment encodings are centralized in
language-local shared packages: `SystemVerilog/rtl/Qrisc_pack.sv`,
`BSV/rtl/Qrisc_pack.bsv`, `BSC/rtl/Qrisc_pack.bs`,
`Clash/src/Qrisc32ISA.hs`, `Amaranth/src/Qrisc32Model/isa.py`,
`ZLang/src/isa.zhl`, and `tools/qrisc32/isa.py`.

## Main Workflow

Run all implemented simulator checks:

```sh
make test
```

Run individual implementation checks:

```sh
make sv-smoke
make sv-flags
make sv-isa
make sv-axi
make bsv-smoke
make bsv-flags
make bsv-isa
make bsv-axi
make amaranth
make clash
make bsc
make zlang
make axi
```

Run optional generic synthesis checks:

```sh
make synth-generic
```

The current generic FPGA synthesis comparison is documented in
`doc/rtl_synthesis_compare.md`.

Each implementation also exposes the same local synthesis entry point:

```sh
make -C SystemVerilog synth
make -C BSV synth
make -C Amaranth synth
make -C Clash synth
make -C BSC synth
```

Run opt-in lint checks without making them part of `make test`:

```sh
make lint
make sv-lint
make bsv-lint
make amaranth-lint
make clash-lint
make bsc-lint
```

Lint targets check source compilers where available and Verilator lint for RTL
or generated HDL.  Generated-code warnings are handled through scoped waiver
files under each implementation's `lint/` directory.

Run opt-in assembler and tiny C compiler checks:

```sh
make asm-tools
make asm-test
make c-test
make toolchain-test
make binary-compat
make sort-test
make sort-binary-compat
```

The qrisc32 toolchain lives under `tools/qrisc32/`. `make binary-compat` builds
the cases in `tests/toolchain_programs.json` and checks the same instruction and
data images on SV, BSV, Amaranth, Clash, BSC, and ZLang. It checks expected
memory, terminal completion, and a non-empty VCD for each case. `make test`
includes this matrix and ZLang's architectural vectors. Generated assembly,
hex, data images, HDL, and VCDs stay under ignored `build/` directories.

The sort programs are maintained as tiny C under `examples/c/`, never as
hand-encoded instructions. `make sort-test` checks bubble, modified-shell, and
first-shell/insertion sorts on the SV reference. `make sort-binary-compat`
runs the same nine generated instruction/data combinations on all six ports.
Use `make update-smoke-golden` only after reviewing an intentional compiler
change; normal generation rejects any mismatch with `tests/smoke.hex`.

The comparison scripts run the public test targets for each implementation:

```sh
bash tools/equiv_smoke.sh
bash tools/equiv_flags.sh
bash tools/equiv_isa.sh
bash tools/equiv_axi.sh
```

The ZLang port uses `ZLANG_ROOT` from the environment when set, or
`zlang` and `python3` on `PATH`. The released Python distribution is
`zlang-hdl==0.1.0a23` (not the unrelated package named `zlang`).
`make zlang-check` checks its source; `make zlang` runs all shared programs on
the native JIT and writes VCDs
under `ZLang/build/vcd/`; `make zlang-verilog` exports RTL under
`ZLang/build/verilog/`, and `make zlang-lint` checks the generated HDL.
The ZLang AXI top uses the updated standard library's full AXI4 field types
with one-bit IDs and USER widths. It supports single-beat transactions only;
see `ZLang/README.md` for its interface scope and toolchain requirement.

For local BSV/BSC tests, set `BSC_HOME` and `PICCOLO_ROOT`. The optional Clash
compiler build uses `CLASH_COMPILER_ROOT`; an installed `clash` on `PATH` also
works. See `.env.example` and `RELEASING.md` for reproducible setup and checks.

## Shared Test Vectors

Shared smoke, flags, ISA, and AXI-smoke program metadata lives in
`tests/vectors.json`.  The referenced hex files stay in `tests/`; implemented
implementations should consume that manifest or generate language-local padded
build images from it without changing the reference vectors.

Reference documentation:

- `tests/README.md`: program intent and expected memory results.
- `doc/isa.md`: encoding notes for the tested ISA subset.

## Adding An Implementation

Add an implementation to `make test` only after it passes the shared smoke,
flags, ISA, hazards, and AXI checks and matches the SystemVerilog results.
