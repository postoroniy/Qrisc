# Qrisc

Qrisc32 is a small 32-bit RISC CPU kept in multiple hardware-description
languages.  The current SystemVerilog implementation is the canonical behavior;
other language versions must match its architectural results before they are
reported as implemented.

## Status

| Directory | Status |
| --- | --- |
| `SystemVerilog/` | Reference five-stage implementation with AXI-style instruction/data interfaces; smoke, flags, ISA, and AXI protocol tests pass with Verilator. |
| `BSV/` | Five-stage Bluespec SystemVerilog port with AXI instruction/data interfaces; smoke, flags, ISA, and AXI protocol tests pass against the shared vectors. |
| `Amaranth/` | Five-stage Amaranth RTL with shared smoke, flags, ISA, and AXI smoke checks passing through generated Verilog; the Python model remains a diagnostic oracle. |
| `Clash/` | Five-stage Clash RTL with AXI instruction/data interfaces; generated-HDL smoke, flags, ISA, and AXI smoke checks are the implementation gate. |
| `BSC/` | Five-stage Bluespec Classic RTL with AXI instruction/data interfaces; Bluesim smoke, flags, ISA, and AXI smoke checks are the implementation gate, with the older model kept as an oracle. |

The old foundry-specific physical-design flow has been removed.  The repository
now keeps simulator tests as the default pass/fail gate, with optional generic
Yosys checks writing only under ignored build directories.

Each language directory keeps source and generated files separate.  Implemented
ports use `rtl/` or `src/` for implementation sources, `tb/` for simulator
testbenches, and `build/` for generated compiler, simulator, waveform, and
synthesis outputs.

All implemented RTL flows export the same public generated-HDL top module name:
`qrisc32`.  Source-language constructors may keep their idiomatic names, but
Verilator and Yosys targets use `qrisc32` consistently.

## Main Workflow

Run all implemented simulator checks:

```sh
make test
```

Run individual parity checks:

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
make axi
```

Run optional generic synthesis checks:

```sh
make synth-generic
```

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

The explicit parity scripts compare the implemented ports through their public
test targets:

```sh
bash tools/equiv_smoke.sh
bash tools/equiv_flags.sh
bash tools/equiv_isa.sh
bash tools/equiv_axi.sh
```

## Shared Test Vectors

Shared smoke, flags, ISA, and AXI-smoke program metadata lives in
`tests/vectors.json`.  The referenced hex files stay in `tests/`; implemented
ports should consume that manifest or generate language-local padded build
images from it without changing the canonical source vectors.

Reference documentation:

- `tests/README.md`: program intent and expected memory results.
- `doc/isa.md`: canonical encoding notes for the currently tested ISA subset.

## Porting Rule

A language directory may exist as a scaffold, but it should not be promoted into
`make test` until it executes the shared smoke, flags, ISA, and AXI-smoke checks
and matches the SystemVerilog architectural results.
