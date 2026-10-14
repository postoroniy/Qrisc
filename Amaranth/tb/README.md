# Amaranth Testbenches

The main Amaranth tests emit Verilog and reuse the SystemVerilog Verilator
testbenches.  These pytest tests are retained as fast diagnostics for the
Python reference model and consume the shared vectors from `../../tests/`.
