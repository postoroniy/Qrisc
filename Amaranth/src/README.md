# Amaranth Source

This directory contains the Amaranth qrisc32 implementation and the retained
Python architectural model.

- `qrisc32_model/rtl.py`: Amaranth RTL with the SV-compatible AXI top port set.
- `qrisc32_model/verilog.py`: Verilog emitter used by `make -C Amaranth verilog`.
- `qrisc32_model/model.py`: Python reference model used by `make -C Amaranth model`.
