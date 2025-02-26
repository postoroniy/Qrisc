export PATH:=/home/slava/projects/bluespec/bsc/inst/bin:$(PATH)

DISTRO ?= /home/slava/projects/bluespec/bsc/inst/lib
BSC = bsc
BSC_PATHS = $(DISTRO)::%/Libraries:.

## Default simulator is iverilog
VSIM = -vsim iverilog
BDIR ?= build
VDIR ?= verilog
IDIR ?= info
SDIR ?= sim

BSC_COMPILATION_FLAGS ?= -keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps -check-assert \
-suppress-warnings G0020 +RTS -K128M -RTS -show-range-conflict -u -elab -bdir $(BDIR)

# Library sources (full paths)
LIB_SRCS := \
	/home/slava/projects/bluespec/Piccolo/src_Core/BSV_Additional_Libs/EdgeFIFOFs.bsv \
	/home/slava/projects/bluespec/Piccolo/src_Core/BSV_Additional_Libs/Semi_FIFOF.bsv \
	/home/slava/projects/bluespec/Piccolo/src_Testbench/Fabrics/AXI4/AXI4_Types.bsv

# Extract only the file names from LIB_SRCS
LIB_SRCS_NAMES := $(notdir $(LIB_SRCS))
# For each source, the corresponding object file is built in $(BDIR) with .bo extension
LIB_OBJS := $(addprefix $(BDIR)/, $(patsubst %.bsv,%.bo,$(LIB_SRCS_NAMES)))

# Top-level Bluespec sources and final verilog output
BSV_SOURCES = qrisc32.bsv
VERILOG_OUTPUT = $(VDIR)/qrisc32.v

all: $(VERILOG_OUTPUT)

# Create required directories.
$(BDIR) $(VDIR) $(IDIR) $(SDIR):
	mkdir -p $@

# Pattern rule: Build a library object from its matching .bsv file.
$(BDIR)/%.bo:
	@# Find the matching source file for $*.bo from LIB_SRCS
	$(eval SRC := $(filter %/$*.bsv, $(LIB_SRCS)))
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -bdir $(BDIR) -info-dir $(IDIR) -vdir $(VDIR) $(SRC)

# Build all library objects.
lib: $(LIB_OBJS)

# Compile intermediate Bluespec files.
compile: lib $(BDIR)
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS):$(BDIR) -info-dir $(IDIR) -vdir $(VDIR) Qrisc_pack.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS):$(BDIR) -info-dir $(IDIR) -vdir $(VDIR) qrisc32_IF.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS):$(BDIR) -info-dir $(IDIR) -vdir $(VDIR) qrisc32_ID.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS):$(BDIR) -info-dir $(IDIR) -vdir $(VDIR) qrisc32_EX.bsv
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS):$(BDIR) -info-dir $(IDIR) -vdir $(VDIR) qrisc32_MEM.bsv

# Build final Verilog output. The top-level module is named after the source file (qrisc32).
$(VERILOG_OUTPUT): $(BSV_SOURCES) | $(BDIR) $(VDIR) $(IDIR) $(SDIR) compile 
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS):$(BDIR) -bdir $(BDIR) -info-dir $(IDIR) -vdir $(VDIR) \
		-remove-dollar -g $(basename $(notdir $(BSV_SOURCES))) -verilog $(BSV_SOURCES)

clean:
	rm -rf ./*.bo ./*.ba ./*.v $(BDIR) $(VDIR) $(IDIR) $(SDIR)

.PHONY: all clean lib
