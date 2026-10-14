# Changelog

## [0.1.0] - 2026-10-14

### Added

- Five-stage, binary-compatible qrisc32 implementations in SystemVerilog,
  BSV, Amaranth, Clash, Bluespec Classic, and ZLang.
- A ZLang implementation using typed native AXI4 interfaces, pipeline intent,
  source assertions and cover goals, native-JIT tests, and generated-Verilog
  linting.
- Shared smoke, flags, ISA, hazard, AXI, and generated-binary compatibility
  gates across all six RTL implementations.
- A repository-local assembler and tiny C compiler with compact scheduling,
  signed operations, and generated sort-program tests.
- Generic synthesis, lint, VCD, and synthesis-comparison workflows.

### Changed

- Software no longer requires fixed hazard-delay NOPs; the implementations
  interlock or serialize dependencies internally.
- Bubble, modified-shell, and first-shell sort images are generated from tiny C
  instead of maintained as hand-encoded binaries.
- Generated outputs and host-specific tool paths are excluded from source
  control.

### Removed

- Sky130/OpenLane-specific collateral and build targets.
- Broken hand-encoded legacy sort binaries and their obsolete SystemVerilog
  loader bench.

[0.1.0]: https://github.com/postoroniy/Qrisc/releases/tag/v0.1.0
