
RTL_DIR=rtl
TB_DIR=tb
CPP_DIR=cpp
BUILD_DIR=build/verilator
VCD_DIR=build/vcd

RTL_SRC=$(RTL_DIR)/risc_pack.sv $(RTL_DIR)/qrisc32_EX.sv $(RTL_DIR)/qrisc32_ID.sv $(RTL_DIR)/qrisc32_IF.sv $(RTL_DIR)/qrisc32_MEM.sv $(RTL_DIR)/qrisc32.sv
SRC=$(TB_DIR)/axi_mem.sv $(RTL_SRC) $(TB_DIR)/qrisc32_tb.sv
SRC_SMOKE=$(TB_DIR)/axi_mem.sv $(RTL_SRC) $(TB_DIR)/qrisc32_smoke_tb.sv
SRC_FLAGS=$(TB_DIR)/axi_mem.sv $(RTL_SRC) $(TB_DIR)/qrisc32_flags_tb.sv
SRC_ISA=$(TB_DIR)/axi_mem.sv $(RTL_SRC) $(TB_DIR)/qrisc32_isa_tb.sv
SRC_AXI=$(TB_DIR)/axi_checked_mem.sv $(RTL_SRC) $(TB_DIR)/qrisc32_axi_tb.sv

TB_BUILD=$(BUILD_DIR)/legacy
SMOKE_BUILD=$(BUILD_DIR)/smoke
FLAGS_BUILD=$(BUILD_DIR)/flags
ISA_BUILD=$(BUILD_DIR)/isa
AXI_BUILD=$(BUILD_DIR)/axi

VERILATOR_CFLAGS ?= -O0 -x-assign 0 --trace -Wno-fatal -Wno-lint -Wno-UNOPTFLAT -I$(RTL_DIR) -I$(TB_DIR)
# --trace-fst
VERILATOR_THREADS ?=2
VERILATOR_ENV ?= CCACHE_DISABLE=1

all: smoke flags isa axi

$(VCD_DIR):
	mkdir -p $(VCD_DIR)

$(TB_BUILD)/Vqrisc32_tb: $(SRC) $(CPP_DIR)/qrisc32_tb.cpp
	mkdir -p $(TB_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC) --Mdir $(TB_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/qrisc32_tb.cpp --top qrisc32_tb

$(VCD_DIR)/qrisc32_tb.vcd: $(TB_BUILD)/Vqrisc32_tb | $(VCD_DIR)
	./$(TB_BUILD)/Vqrisc32_tb
	test -s $(VCD_DIR)/qrisc32_tb.vcd

$(SMOKE_BUILD)/Vqrisc32_smoke_tb: $(SRC_SMOKE) $(CPP_DIR)/qrisc32_smoke_tb.cpp
	mkdir -p $(SMOKE_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_SMOKE) --Mdir $(SMOKE_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/qrisc32_smoke_tb.cpp --top qrisc32_smoke_tb

smoke: $(TB_BUILD)/Vqrisc32_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_tb.vcd
	./$(TB_BUILD)/Vqrisc32_tb
	test -s $(VCD_DIR)/qrisc32_tb.vcd

$(FLAGS_BUILD)/Vqrisc32_flags_tb: $(SRC_FLAGS) $(CPP_DIR)/qrisc32_flags_tb.cpp
	mkdir -p $(FLAGS_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_FLAGS) --Mdir $(FLAGS_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/qrisc32_flags_tb.cpp --top qrisc32_flags_tb

flags: $(FLAGS_BUILD)/Vqrisc32_flags_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_flags_tb.vcd
	./$(FLAGS_BUILD)/Vqrisc32_flags_tb
	test -s $(VCD_DIR)/qrisc32_flags_tb.vcd

$(ISA_BUILD)/Vqrisc32_isa_tb: $(SRC_ISA) $(CPP_DIR)/qrisc32_isa_tb.cpp
	mkdir -p $(ISA_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_ISA) --Mdir $(ISA_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/qrisc32_isa_tb.cpp --top qrisc32_isa_tb

isa: $(ISA_BUILD)/Vqrisc32_isa_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_isa_tb.vcd
	./$(ISA_BUILD)/Vqrisc32_isa_tb
	test -s $(VCD_DIR)/qrisc32_isa_tb.vcd

$(AXI_BUILD)/Vqrisc32_axi_tb: $(SRC_AXI) $(CPP_DIR)/qrisc32_axi_tb.cpp
	mkdir -p $(AXI_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SRC_AXI) --Mdir $(AXI_BUILD) --threads $(VERILATOR_THREADS) --build $(CPP_DIR)/qrisc32_axi_tb.cpp --top qrisc32_axi_tb

axi: $(AXI_BUILD)/Vqrisc32_axi_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_axi_tb.vcd
	./$(AXI_BUILD)/Vqrisc32_axi_tb
	test -s $(VCD_DIR)/qrisc32_axi_tb.vcd

synth: $(RTL_SRC) ys
	mkdir -p build/yosys
	yosys -s ys

clean:
	rm -rf build *.vcd obj_dir yosys_qrisc32*

.PHONY: all smoke flags isa axi synth clean
