PYTHON ?= python3
BUILD_DIR ?= ../build/toolchain
SV_DIR ?= ../SystemVerilog
QRISC_AS = PYTHONPATH=. $(PYTHON) ./qrisc32-as
QRISC_CC = PYTHONPATH=. $(PYTHON) ./qrisc32-cc
UNIT = PYTHONPATH=. $(PYTHON) -m unittest discover -s tests
SV_BENCH = $(SV_DIR)/build/verilator/toolchain/Vqrisc32_toolchain_tb
SV_BENCH_DEPS = $(SV_DIR)/rtl/Qrisc32.sv \
	$(SV_DIR)/rtl/Qrisc32EX.sv \
	$(SV_DIR)/rtl/Qrisc32ID.sv \
	$(SV_DIR)/rtl/Qrisc32IF.sv \
	$(SV_DIR)/rtl/Qrisc32MEM.sv \
	$(SV_DIR)/rtl/Qrisc32RF.sv \
	$(SV_DIR)/rtl/Qrisc32WB.sv \
	$(SV_DIR)/rtl/Qrisc_pack.sv \
	$(SV_DIR)/tb/Qrisc32ToolchainTb.sv \
	$(SV_DIR)/cpp/Qrisc32ToolchainTb.cpp

.PHONY: asm-tools unit asm-test c-test sort-test toolchain-test \
	check-smoke-golden update-smoke-golden clean

asm-tools:
	$(PYTHON) -m py_compile qrisc32/isa.py qrisc32/encoder.py qrisc32/asm.py qrisc32/cc.py \
		qrisc32/check_nops.py qrisc32/generate_programs.py qrisc32/run_binary_compat.py

unit: asm-tools
	$(UNIT)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/asm_flags.hex: ../examples/asm/flags.s qrisc32/asm.py qrisc32/encoder.py | $(BUILD_DIR)
	$(QRISC_AS) $< -o $@ --dmem $(BUILD_DIR)/asm_flags.dmem.hex

$(BUILD_DIR)/asm_axi_smoke.hex: ../examples/asm/axi_smoke.s qrisc32/asm.py qrisc32/encoder.py | $(BUILD_DIR)
	$(QRISC_AS) $< -o $@ --dmem $(BUILD_DIR)/asm_axi_smoke.dmem.hex

$(BUILD_DIR)/c_flags.s $(BUILD_DIR)/c_flags.hex $(BUILD_DIR)/c_flags.dmem.hex: ../examples/c/flags.c qrisc32/cc.py qrisc32/asm.py qrisc32/encoder.py | $(BUILD_DIR)
	$(QRISC_CC) $< -S -o $(BUILD_DIR)/c_flags.s --hex $(BUILD_DIR)/c_flags.hex --dmem $(BUILD_DIR)/c_flags.dmem.hex

$(BUILD_DIR)/c_bubble_sort.s $(BUILD_DIR)/c_bubble_sort.hex $(BUILD_DIR)/c_bubble_sort.dmem.hex: ../examples/c/bubble_sort.c qrisc32/cc.py qrisc32/asm.py qrisc32/encoder.py | $(BUILD_DIR)
	$(QRISC_CC) $< -S -o $(BUILD_DIR)/c_bubble_sort.s --hex $(BUILD_DIR)/c_bubble_sort.hex --dmem $(BUILD_DIR)/c_bubble_sort.dmem.hex

$(BUILD_DIR)/c_signed.s $(BUILD_DIR)/c_signed.hex $(BUILD_DIR)/c_signed.dmem.hex: ../examples/c/signed.c qrisc32/cc.py qrisc32/asm.py qrisc32/encoder.py | $(BUILD_DIR)
	$(QRISC_CC) $< -S -o $(BUILD_DIR)/c_signed.s --hex $(BUILD_DIR)/c_signed.hex --dmem $(BUILD_DIR)/c_signed.dmem.hex

$(SV_BENCH): $(SV_BENCH_DEPS)
	$(MAKE) -C $(SV_DIR) toolchain-bench

asm-test: unit $(SV_BENCH) $(BUILD_DIR)/asm_flags.hex $(BUILD_DIR)/asm_axi_smoke.hex
	$(SV_BENCH) +NAME=asm_flags +IMEM=$(abspath $(BUILD_DIR)/asm_flags.hex) +EXPECT_COUNT=2 +EXPECT0=5 +EXPECT1=7 +TIMEOUT=3000 +VCD=$(abspath $(BUILD_DIR)/asm_flags.vcd)
	$(SV_BENCH) +NAME=asm_axi_smoke +IMEM=$(abspath $(BUILD_DIR)/asm_axi_smoke.hex) +EXPECT_COUNT=2 +EXPECT0=12 +EXPECT1=12 +TIMEOUT=3000 +VCD=$(abspath $(BUILD_DIR)/asm_axi_smoke.vcd)

c-test: unit $(SV_BENCH) $(BUILD_DIR)/c_flags.hex $(BUILD_DIR)/c_bubble_sort.hex $(BUILD_DIR)/c_signed.hex
	PYTHONPATH=. $(PYTHON) -m qrisc32.check_nops --max-run 5 $(BUILD_DIR)/c_flags.s $(BUILD_DIR)/c_bubble_sort.s $(BUILD_DIR)/c_signed.s
	$(SV_BENCH) +NAME=c_flags +IMEM=$(abspath $(BUILD_DIR)/c_flags.hex) +DMEM=$(abspath $(BUILD_DIR)/c_flags.dmem.hex) +EXPECT_COUNT=2 +EXPECT0=5 +EXPECT1=7 +TIMEOUT=8000 +VCD=$(abspath $(BUILD_DIR)/c_flags.vcd)
	$(SV_BENCH) +NAME=c_bubble_sort +IMEM=$(abspath $(BUILD_DIR)/c_bubble_sort.hex) +DMEM=$(abspath $(BUILD_DIR)/c_bubble_sort.dmem.hex) +EXPECT_COUNT=10 +EXPECT0=1 +EXPECT1=2 +EXPECT2=3 +EXPECT3=4 +EXPECT4=5 +EXPECT5=6 +EXPECT6=7 +EXPECT7=8 +EXPECT8=9 +EXPECT9=10 +TIMEOUT=250000 +VCD=$(abspath $(BUILD_DIR)/c_bubble_sort.vcd)
	PYTHONPATH=. $(PYTHON) -m qrisc32.run_binary_compat SystemVerilog --vector c_signed

check-smoke-golden: unit
	PYTHONPATH=. $(PYTHON) -m qrisc32.generate_programs

update-smoke-golden: unit
	PYTHONPATH=. $(PYTHON) -m qrisc32.generate_programs --update-smoke-golden

sort-test: check-smoke-golden
	PYTHONPATH=. $(PYTHON) -m qrisc32.run_binary_compat SystemVerilog --group sort

toolchain-test: asm-test c-test sort-test

clean:
	rm -rf $(BUILD_DIR)
