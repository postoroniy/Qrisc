# Amaranth Source

This directory contains the Amaranth qrisc32 implementation and the retained
Python architectural model.

- `Qrisc32Model/rtl.py`: five-stage Amaranth RTL with the SV-compatible AXI top port set.
- `Qrisc32Model/verilog.py`: Verilog emitter used by `make -C Amaranth verilog`.
- `Qrisc32Model/model.py`: Python reference model used by `make -C Amaranth model`.
