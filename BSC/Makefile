export PATH:=${HOME}/projects/bluespec/bsc/inst/bin:$(PATH)

DISTRO ?= ${HOME}/projects/bluespec/bsc/inst/lib
BSC ?= bsc
BSV_LIBS ?= ${HOME}/projects/bluespec/Piccolo
RTL_DIR ?= rtl
TB_DIR ?= tb
BUILD_DIR ?= build
BSC_PATHS = $(DISTRO)::%/Libraries:$(RTL_DIR):src:$(TB_DIR):$(BSV_LIBS)/src_Core/BSV_Additional_Libs:$(BSV_LIBS)/src_Testbench/Fabrics/AXI4:.
BDIR ?= $(BUILD_DIR)
IDIR ?= $(BDIR)/info
VDIR ?= $(BDIR)/verilog
SIMDIR ?= $(BDIR)/bluesim
SDIR ?= $(BDIR)/sim
HEXDIR ?= $(BDIR)/hex
VCD_DIR ?= $(BDIR)/vcd

SMOKE_HEX = $(HEXDIR)/smoke_1024.hex
SMOKE_INIT_HEX = $(HEXDIR)/smoke_reversed_1024.hex
AXI_HEX = $(HEXDIR)/axi_smoke_1024.hex
FLAGS_HEX = $(HEXDIR)/flags_1024.hex
ISA_HEX = $(HEXDIR)/isa_1024.hex
DMEM_ZERO_HEX = $(HEXDIR)/dmem_zero_1024.hex
RF_ZERO_HEX = $(HEXDIR)/rf_zero_32.hex

BSC_FLAGS ?= -keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps \
	-check-assert -suppress-warnings G0020 +RTS -K128M -RTS -show-range-conflict
BSC_SIM_FLAGS = -sim $(BSC_FLAGS) -u -elab -bdir $(BDIR) -simdir $(SIMDIR) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR)
BSC_VERILOG_FLAGS = $(BSC_FLAGS) -u -elab -bdir $(BDIR) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR)
VERILOG_OUTPUT = $(VDIR)/mkQrisc32.v

.PHONY: all smoke flags isa axi test model model-smoke model-flags model-isa model-axi verilog synth clean

all: smoke flags isa axi
test: all

$(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(HEXDIR) $(VCD_DIR):
	mkdir -p $@

$(HEXDIR)/%_1024.hex: ../tests/%.hex | $(HEXDIR)
	awk 'BEGIN { n = 0 } { print; n++ } END { for (; n < 1024; n++) print "00000000" }' $< > $@

$(RF_ZERO_HEX): | $(HEXDIR)
	awk 'BEGIN { for (n = 0; n < 32; n++) print "00000000" }' > $@

define RUN_BSC_TEST
	$(BSC) $(BSC_SIM_FLAGS) -g $(1) $(TB_DIR)/$(2).bs
	$(BSC) -sim -e $(1) -bdir $(BDIR) -simdir $(SIMDIR) -o $(SDIR)/$(1)
	@log="$(BDIR)/$(1).log"; \
	vcd="$(VCD_DIR)/$(1).vcd"; \
	rm -f "$$vcd"; \
	$(SDIR)/$(1) -V "$$vcd" > "$$log" 2>&1; \
	status=$$?; \
	cat "$$log"; \
	if [ $$status -ne 0 ]; then exit $$status; fi; \
	grep -q '$(3)' "$$log"; \
	test -s "$$vcd"
endef

smoke: $(SMOKE_HEX) $(SMOKE_INIT_HEX) | $(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkSmokeTest,SmokeTest,PASS: qrisc32 sort smoke)

flags: $(FLAGS_HEX) | $(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkFlagsTest,FlagsTest,PASS: dmem\[0\]=5 dmem\[1\]=7)

isa: $(ISA_HEX) $(DMEM_ZERO_HEX) | $(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkIsaTest,IsaTest,PASS: ISA parity vector)

axi: $(AXI_HEX) | $(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkAxiTest,AxiTest,PASS: AXI smoke dmem\[0\] = 12)

model: model-smoke model-flags model-isa model-axi

model-smoke: $(SMOKE_HEX) $(SMOKE_INIT_HEX) $(RF_ZERO_HEX) | $(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkModelSmokeTest,ModelSmokeTest,PASS: qrisc32 sort smoke)

model-flags: $(FLAGS_HEX) $(RF_ZERO_HEX) | $(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkModelFlagsTest,ModelFlagsTest,PASS: dmem\[0\]=5 dmem\[1\]=7)

model-isa: $(ISA_HEX) $(DMEM_ZERO_HEX) $(RF_ZERO_HEX) | $(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkModelIsaTest,ModelIsaTest,PASS: ISA parity vector)

model-axi: $(AXI_HEX) $(RF_ZERO_HEX) | $(BDIR) $(IDIR) $(VDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkModelAxiTest,ModelAxiTest,PASS: AXI smoke dmem\[0\] = 12)

$(VERILOG_OUTPUT): $(RTL_DIR)/Qrisc32RTL.bs | $(BDIR) $(IDIR) $(VDIR)
	$(BSC) $(BSC_VERILOG_FLAGS) -remove-dollar -g mkQrisc32 -verilog $(RTL_DIR)/Qrisc32RTL.bs
	test -s $(VERILOG_OUTPUT)

verilog: $(VERILOG_OUTPUT)

synth: verilog
	mkdir -p $(BDIR)/yosys
	yosys -s ys

clean:
	rm -rf $(BDIR)
