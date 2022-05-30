
SRC=mem.sv risc_pack.sv qrisc32_EX.sv qrisc32_ID.sv qrisc32_IF.sv qrisc32_MEM.sv qrisc32.sv qrisc32_tb.sv

VERILATOR_CFLAGS ?= -Os -x-assign 0 --trace --trace-fst -Wno-fatal -Wno-lint -Wno-UNOPTFLAT
VERILATOR_THREADS ?=2

all: qrisc32_tb.vcd yosys_qrisc32_syn.v

obj_dir/Vqrisc32_tb: $(SRC) qrisc32_tb.cpp
	verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC) --threads $(VERILATOR_THREADS) --build qrisc32_tb.cpp --top qrisc32_tb

qrisc32_tb.vcd: obj_dir/Vqrisc32_tb
	./obj_dir/Vqrisc32_tb

yosys_qrisc32_syn.v:
	yosys -s ys

clean:
	rm -rf yosys_qrisc32_syn.v *.vcd obj_dir
