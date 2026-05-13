export PATH:=${HOME}/projects/bluespec/bsc/inst/bin:$(PATH)

DISTRO ?= ${HOME}/projects/bluespec/bsc/inst/lib
BSV_LIBS ?= ${HOME}/projects/bluespec/Piccolo

BSC = bsc
VERILATOR ?= verilator
RTL_DIR = rtl
TB_DIR = tb
BUILD_DIR = build

BSC_PATHS = $(DISTRO)::%/Libraries:$(RTL_DIR):$(TB_DIR):.

## Default simulator is iverilog
VSIM = -vsim iverilog
BDIR ?= $(BUILD_DIR)
VDIR ?= $(BDIR)/verilog
IDIR ?= $(BDIR)/info
SDIR ?= $(BDIR)/sim
SIMDIR ?= $(BDIR)/bluesim
HEXDIR ?= $(BDIR)/hex
VCD_DIR ?= $(BDIR)/vcd

BSC_COMPILATION_FLAGS ?= -keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps -check-assert \
-suppress-warnings G0020 +RTS -K128M -RTS -show-range-conflict -u -elab -bdir $(BDIR)
BSC_SIM_FLAGS ?= $(BSC_COMPILATION_FLAGS) -simdir $(SIMDIR)
BSC_SOURCE_LINT_FLAGS ?= -keep-fires -aggressive-conditions -no-show-timestamps -check-assert \
+RTS -K128M -RTS -show-range-conflict -u -elab -bdir $(BDIR)
VERILATOR_LINT_FLAGS ?= --lint-only -Wall
LINT_WAIVERS ?= lint/verilator.vlt

# Library sources (full paths)
LIB_SRCS := \
	$(BSV_LIBS)/src_Core/BSV_Additional_Libs/EdgeFIFOFs.bsv \
	$(BSV_LIBS)/src_Core/BSV_Additional_Libs/Semi_FIFOF.bsv \
	$(BSV_LIBS)/src_Testbench/Fabrics/AXI4/AXI4_Types.bsv

# Extract only the file names from LIB_SRCS
LIB_SRCS_NAMES := $(notdir $(LIB_SRCS))
# For each source, the corresponding object file is built in $(BDIR) with .bo extension
LIB_OBJS := $(addprefix $(BDIR)/, $(patsubst %.bsv,%.bo,$(LIB_SRCS_NAMES)))

# Top-level Bluespec sources and final verilog output
RTL_SOURCES = \
	$(RTL_DIR)/Qrisc_pack.bsv \
	$(RTL_DIR)/Qrisc32IF.bsv \
	$(RTL_DIR)/Qrisc32ID.bsv \
	$(RTL_DIR)/Qrisc32EX.bsv \
	$(RTL_DIR)/Qrisc32MEM.bsv \
	$(RTL_DIR)/Qrisc32RF.bsv \
	$(RTL_DIR)/Qrisc32WB.bsv \
	$(RTL_DIR)/Qrisc32.bsv
BSV_SOURCES = $(RTL_DIR)/Qrisc32.bsv
BSV_TOP = qrisc32
VERILOG_OUTPUT = $(VDIR)/qrisc32.v

SMOKE_HEX = $(HEXDIR)/smoke_1024.hex
SMOKE_INIT_HEX = $(HEXDIR)/smoke_reversed_1024.hex
AXI_HEX = $(HEXDIR)/axi_smoke_1024.hex

all: smoke flags isa axi

# Create required directories.
$(BDIR) $(VDIR) $(IDIR) $(SDIR) $(SIMDIR) $(HEXDIR) $(VCD_DIR):
	mkdir -p $@

$(HEXDIR)/%_1024.hex: ../tests/%.hex | $(HEXDIR)
	awk 'BEGIN { n = 0 } { print; n++ } END { for (; n < 1024; n++) print "00000000" }' $< > $@

# Pattern rule: Build a library object from its matching .bsv file.
$(BDIR)/%.bo: | $(BDIR) $(IDIR) $(VDIR)
	@# Find the matching source file for $*.bo from LIB_SRCS
	$(eval SRC := $(filter %/$*.bsv, $(LIB_SRCS)))
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) $(SRC)

# Build all library objects.
lib: $(LIB_OBJS)

# Compile intermediate Bluespec files.
compile: lib $(RTL_SOURCES) | $(BDIR) $(IDIR) $(VDIR)
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) $(RTL_DIR)/Qrisc_pack.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) $(RTL_DIR)/Qrisc32IF.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) $(RTL_DIR)/Qrisc32ID.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) $(RTL_DIR)/Qrisc32EX.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) $(RTL_DIR)/Qrisc32MEM.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) $(RTL_DIR)/Qrisc32RF.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) $(RTL_DIR)/Qrisc32WB.bsv

# Build final Verilog output. Keep the public top module name stable.
$(VERILOG_OUTPUT): $(RTL_SOURCES) | $(BDIR) $(VDIR) $(IDIR) $(SDIR) $(SIMDIR) compile
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -vdir $(VDIR) \
		-remove-dollar -g $(BSV_TOP) -verilog $(BSV_SOURCES)

synth: $(VERILOG_OUTPUT)
	mkdir -p $(BDIR)/yosys
	yosys -s ys

lint: BSC_COMPILATION_FLAGS = $(BSC_SOURCE_LINT_FLAGS)
lint: $(VERILOG_OUTPUT) $(LINT_WAIVERS)
	$(VERILATOR) $(VERILATOR_LINT_FLAGS) $(LINT_WAIVERS) \
		$(DISTRO)/Verilog/FIFO1.v $(DISTRO)/Verilog/FIFO2.v $(VERILOG_OUTPUT) --top-module qrisc32

clean:
	rm -rf *.bo *.ba *.v *.cxx *.h *.o *.so info sim verilog $(BDIR)

smoke: lib compile $(SMOKE_HEX) $(SMOKE_INIT_HEX) | $(SDIR) $(BDIR) $(IDIR) $(SIMDIR) $(VCD_DIR)
	$(BSC) -sim $(BSC_SIM_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -g mkTestbench $(TB_DIR)/mkTestbench.bsv
	$(BSC) -sim -e mkTestbench -bdir $(BDIR) -simdir $(SIMDIR) -o $(SDIR)/mkTestbench
	@log="$(BDIR)/mkTestbench.log"; \
	vcd="$(VCD_DIR)/mkTestbench.vcd"; \
	rm -f "$$vcd"; \
	$(SDIR)/mkTestbench -V "$$vcd" > "$$log" 2>&1; \
	status=$$?; \
	cat "$$log"; \
	if [ $$status -ne 0 ]; then exit $$status; fi; \
	grep -q 'PASS: qrisc32 sort smoke' "$$log"; \
	test -s "$$vcd"

flags: lib compile | $(SDIR) $(BDIR) $(IDIR) $(SIMDIR) $(VCD_DIR)
	$(BSC) -sim $(BSC_SIM_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -g mkFlagsTestbench $(TB_DIR)/mkFlagsTestbench.bsv
	$(BSC) -sim -e mkFlagsTestbench -bdir $(BDIR) -simdir $(SIMDIR) -o $(SDIR)/mkFlagsTestbench
	@log="$(BDIR)/mkFlagsTestbench.log"; \
	vcd="$(VCD_DIR)/mkFlagsTestbench.vcd"; \
	rm -f "$$vcd"; \
	$(SDIR)/mkFlagsTestbench -V "$$vcd" > "$$log" 2>&1; \
	status=$$?; \
	cat "$$log"; \
	if [ $$status -ne 0 ]; then exit $$status; fi; \
	grep -q 'PASS: dmem\[0\]=5 dmem\[1\]=7' "$$log"; \
	test -s "$$vcd"

isa: lib compile | $(SDIR) $(BDIR) $(IDIR) $(SIMDIR) $(VCD_DIR)
	$(BSC) -sim $(BSC_SIM_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -g mkIsaTestbench $(TB_DIR)/mkIsaTestbench.bsv
	$(BSC) -sim -e mkIsaTestbench -bdir $(BDIR) -simdir $(SIMDIR) -o $(SDIR)/mkIsaTestbench
	@log="$(BDIR)/mkIsaTestbench.log"; \
	vcd="$(VCD_DIR)/mkIsaTestbench.vcd"; \
	rm -f "$$vcd"; \
	$(SDIR)/mkIsaTestbench -V "$$vcd" > "$$log" 2>&1; \
	status=$$?; \
	cat "$$log"; \
	if [ $$status -ne 0 ]; then exit $$status; fi; \
	grep -q 'PASS: ISA parity vector' "$$log"; \
	test -s "$$vcd"

axi: lib compile $(AXI_HEX) | $(SDIR) $(BDIR) $(IDIR) $(SIMDIR) $(VCD_DIR)
	$(BSC) -sim $(BSC_SIM_FLAGS) -p $(BSC_PATHS) -info-dir $(IDIR) -g mkAxiTestbench $(TB_DIR)/mkAxiTestbench.bsv
	$(BSC) -sim -e mkAxiTestbench -bdir $(BDIR) -simdir $(SIMDIR) -o $(SDIR)/mkAxiTestbench
	@log="$(BDIR)/mkAxiTestbench.log"; \
	vcd="$(VCD_DIR)/mkAxiTestbench.vcd"; \
	rm -f "$$vcd"; \
	$(SDIR)/mkAxiTestbench -V "$$vcd" > "$$log" 2>&1; \
	status=$$?; \
	cat "$$log"; \
	if [ $$status -ne 0 ]; then exit $$status; fi; \
	grep -q 'PASS: AXI smoke dmem\[0\] = 12' "$$log"; \
	test -s "$$vcd"

.PHONY: all clean compile lib synth lint smoke flags isa axi
