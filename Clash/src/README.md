# Clash Source

`Qrisc32RTL.hs` is the synthesizable five-stage Clash qrisc32 implementation.
It exposes the same single-beat AXI instruction and data master contract used
by the SystemVerilog reference, and generates the same public HDL top module
name: `qrisc32`.

`Qrisc32Model.hs` is retained as a non-synthesizable architectural regression
model behind the `model-*` make targets.
