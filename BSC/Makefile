export PATH:=${HOME}/projects/bluespec/bsc/inst/bin:$(PATH)

DISTRO ?= ${HOME}/projects/bluespec/bsc/inst/lib
BSC ?= bsc
BSC_PATHS = $(DISTRO)::%/Libraries:src:tb:.
BDIR ?= build
IDIR ?= $(BDIR)/info
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
BSC_SIM_FLAGS = -sim $(BSC_FLAGS) -u -elab -bdir $(BDIR) -simdir $(SIMDIR) -p $(BSC_PATHS) -info-dir $(IDIR)

.PHONY: all smoke flags isa axi test clean

all: smoke flags isa axi
test: all

$(BDIR) $(IDIR) $(SIMDIR) $(SDIR) $(HEXDIR) $(VCD_DIR):
	mkdir -p $@

$(HEXDIR)/%_1024.hex: ../tests/%.hex | $(HEXDIR)
	awk 'BEGIN { n = 0 } { print; n++ } END { for (; n < 1024; n++) print "00000000" }' $< > $@

$(RF_ZERO_HEX): | $(HEXDIR)
	awk 'BEGIN { for (n = 0; n < 32; n++) print "00000000" }' > $@

define RUN_BSC_TEST
	$(BSC) $(BSC_SIM_FLAGS) -g $(1) tb/$(2).bs
	$(BSC) -sim -e $(1) -bdir $(BDIR) -simdir $(SIMDIR) -o $(SDIR)/$(1)
	rm -f $(VCD_DIR)/$(1).vcd
	$(SDIR)/$(1) -V $(VCD_DIR)/$(1).vcd
	test -s $(VCD_DIR)/$(1).vcd
endef

smoke: $(SMOKE_HEX) $(SMOKE_INIT_HEX) $(RF_ZERO_HEX) | $(BDIR) $(IDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkSmokeTest,SmokeTest)

flags: $(FLAGS_HEX) $(RF_ZERO_HEX) | $(BDIR) $(IDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkFlagsTest,FlagsTest)

isa: $(ISA_HEX) $(DMEM_ZERO_HEX) $(RF_ZERO_HEX) | $(BDIR) $(IDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkIsaTest,IsaTest)

axi: $(AXI_HEX) $(RF_ZERO_HEX) | $(BDIR) $(IDIR) $(SIMDIR) $(SDIR) $(VCD_DIR)
	$(call RUN_BSC_TEST,mkAxiTest,AxiTest)

clean:
	rm -rf $(BDIR)
