# Bluespec Classic RTL

This directory contains the implemented five-stage qrisc32 RTL in Bluespec
Classic syntax. Bluespec Classic sources use the `.bs` extension.

`Qrisc_pack.bs` is the shared BSC ISA package for opcode, sub-opcode, flag,
jump, and increment enums.  RTL code imports those constrained types instead of
spelling instruction encodings locally.

The public BSC tests instantiate `mkQrisc32`, which exposes separate AXI-style
instruction-read and data read/write master interfaces.  Generated Verilog and
synthesis use the `qrisc32` wrapper so the public HDL top name matches the
SystemVerilog reference.
