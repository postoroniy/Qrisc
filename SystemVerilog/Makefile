
RTL_DIR=rtl
TB_DIR=tb
CPP_DIR=cpp
BUILD_DIR=build/verilator
VCD_DIR=build/vcd

RTL_SRC=$(RTL_DIR)/Qrisc_pack.sv $(RTL_DIR)/Qrisc32RF.sv $(RTL_DIR)/Qrisc32WB.sv $(RTL_DIR)/Qrisc32EX.sv $(RTL_DIR)/Qrisc32ID.sv $(RTL_DIR)/Qrisc32IF.sv $(RTL_DIR)/Qrisc32MEM.sv $(RTL_DIR)/Qrisc32.sv
SRC_SMOKE=$(TB_DIR)/axi_mem.sv $(RTL_SRC) $(TB_DIR)/Qrisc32SmokeTb.sv
SRC_FLAGS=$(TB_DIR)/axi_mem.sv $(RTL_SRC) $(TB_DIR)/Qrisc32FlagsTb.sv
SRC_ISA=$(TB_DIR)/axi_mem.sv $(RTL_SRC) $(TB_DIR)/Qrisc32IsaTb.sv
SRC_AXI=$(TB_DIR)/axi_checked_mem.sv $(RTL_SRC) $(TB_DIR)/Qrisc32AxiTb.sv
SRC_TOOLCHAIN=$(TB_DIR)/axi_mem.sv $(RTL_SRC) $(TB_DIR)/Qrisc32ToolchainTb.sv

SMOKE_BUILD=$(BUILD_DIR)/smoke
FLAGS_BUILD=$(BUILD_DIR)/flags
ISA_BUILD=$(BUILD_DIR)/isa
AXI_BUILD=$(BUILD_DIR)/axi
TOOLCHAIN_BUILD=$(BUILD_DIR)/toolchain
HAZARDS_BUILD=$(BUILD_DIR)/hazards
GEN_TB_DIR=build/tb
HAZARDS_TB_DIR=$(GEN_TB_DIR)/hazards

VERILATOR_CFLAGS ?= -O0 -x-assign 0 --trace -Wno-fatal -Wno-lint -Wno-UNOPTFLAT -I$(RTL_DIR) -I$(TB_DIR)
VERILATOR_LINT_FLAGS ?= --lint-only -Wall -I$(RTL_DIR) -I$(TB_DIR)
LINT_WAIVERS ?= lint/verilator.vlt
# --trace-fst
VERILATOR_THREADS ?=2
VERILATOR_ENV ?= CCACHE_DISABLE=1

all: smoke flags isa axi hazards

$(VCD_DIR) $(HAZARDS_TB_DIR):
	mkdir -p $@

$(SMOKE_BUILD)/Vqrisc32_smoke_tb: $(SRC_SMOKE) $(CPP_DIR)/Qrisc32SmokeTb.cpp
	mkdir -p $(SMOKE_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_SMOKE) --Mdir $(SMOKE_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/Qrisc32SmokeTb.cpp --top qrisc32_smoke_tb

smoke: $(SMOKE_BUILD)/Vqrisc32_smoke_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_smoke_tb.vcd
	./$(SMOKE_BUILD)/Vqrisc32_smoke_tb
	test -s $(VCD_DIR)/qrisc32_smoke_tb.vcd

$(FLAGS_BUILD)/Vqrisc32_flags_tb: $(SRC_FLAGS) $(CPP_DIR)/Qrisc32FlagsTb.cpp
	mkdir -p $(FLAGS_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_FLAGS) --Mdir $(FLAGS_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/Qrisc32FlagsTb.cpp --top qrisc32_flags_tb

flags: $(FLAGS_BUILD)/Vqrisc32_flags_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_flags_tb.vcd
	./$(FLAGS_BUILD)/Vqrisc32_flags_tb
	test -s $(VCD_DIR)/qrisc32_flags_tb.vcd

$(ISA_BUILD)/Vqrisc32_isa_tb: $(SRC_ISA) $(CPP_DIR)/Qrisc32IsaTb.cpp
	mkdir -p $(ISA_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_ISA) --Mdir $(ISA_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/Qrisc32IsaTb.cpp --top qrisc32_isa_tb

isa: $(ISA_BUILD)/Vqrisc32_isa_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_isa_tb.vcd
	./$(ISA_BUILD)/Vqrisc32_isa_tb
	test -s $(VCD_DIR)/qrisc32_isa_tb.vcd

$(AXI_BUILD)/Vqrisc32_axi_tb: $(SRC_AXI) $(CPP_DIR)/Qrisc32AxiTb.cpp
	mkdir -p $(AXI_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_AXI) --Mdir $(AXI_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/Qrisc32AxiTb.cpp --top qrisc32_axi_tb

axi: $(AXI_BUILD)/Vqrisc32_axi_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_axi_tb.vcd
	./$(AXI_BUILD)/Vqrisc32_axi_tb
	test -s $(VCD_DIR)/qrisc32_axi_tb.vcd

$(TOOLCHAIN_BUILD)/Vqrisc32_toolchain_tb: $(SRC_TOOLCHAIN) $(CPP_DIR)/Qrisc32ToolchainTb.cpp
	mkdir -p $(TOOLCHAIN_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_TOOLCHAIN) --Mdir $(TOOLCHAIN_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/Qrisc32ToolchainTb.cpp --top qrisc32_toolchain_tb

toolchain-bench: $(TOOLCHAIN_BUILD)/Vqrisc32_toolchain_tb

$(HAZARDS_TB_DIR)/.stamp: ../Clash/tb/gen_vector_bench.py ../tests/vectors.json | $(HAZARDS_TB_DIR)
	python3 ../Clash/tb/gen_vector_bench.py --vector hazards --manifest ../tests/vectors.json --out-dir $(HAZARDS_TB_DIR) --vcd-dir $(VCD_DIR)
	touch $@

$(HAZARDS_BUILD)/Vqrisc32_hazards_tb: $(TB_DIR)/axi_checked_mem.sv $(RTL_SRC) $(HAZARDS_TB_DIR)/.stamp
	mkdir -p $(HAZARDS_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) -DQRISC32_SV_HAZARD_MONITOR $(TB_DIR)/axi_checked_mem.sv $(RTL_SRC) $(HAZARDS_TB_DIR)/qrisc32_hazards_tb.sv --Mdir $(HAZARDS_BUILD) --threads $(VERILATOR_THREADS) --build $(HAZARDS_TB_DIR)/qrisc32_hazards_tb.cpp --top qrisc32_hazards_tb

hazards: $(HAZARDS_BUILD)/Vqrisc32_hazards_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/hazards.vcd
	./$(HAZARDS_BUILD)/Vqrisc32_hazards_tb
	test -s $(VCD_DIR)/hazards.vcd

synth: $(RTL_SRC) ys
	mkdir -p build/yosys
	yosys -s ys

lint: $(RTL_SRC) $(LINT_WAIVERS)
	$(VERILATOR_ENV) verilator $(VERILATOR_LINT_FLAGS) $(LINT_WAIVERS) $(RTL_SRC) --top-module qrisc32

clean:
	rm -rf build *.vcd obj_dir yosys_qrisc32*

.PHONY: all smoke flags isa axi hazards toolchain-bench synth lint clean
