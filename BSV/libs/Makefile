toplevel_module = mkTop
default: all

export PATH:=${HOME}/projects/bluespec/bsc/inst/bin:$(PATH)
DISTRO ?= ${HOME}/projects/bluespec/bsc/inst/lib
BSC = bsc
BSC_PATHS = $(DISTRO)::%/Libraries:.

BSC_FLAGS = -u
BSC_VER_OPTS = -elab
BSC_SIM_OPTS = -sim

BDIR ?= build
SDIR ?= sim

# Ensure mkTop.bsv is a dependency for the build
$(BDIR)/$(toplevel_module).ba: $(BDIR) $(toplevel_module).bsv mkMultiCycleFunction.bsv
	$(BSC) $(BSC_SIM_OPTS) $(BSC_FLAGS) -p $(BSC_PATHS) -bdir $(BDIR) -g $(toplevel_module) $(toplevel_module).bsv

$(toplevel_module): $(BDIR)/$(toplevel_module).ba $(SDIR)
	$(BSC) $(BSC_SIM_OPTS) -simdir $(SDIR) -e $(toplevel_module) -p $(BSC_PATHS) -bdir $(BDIR) -o $(toplevel_module)

all: $(toplevel_module).vcd

$(toplevel_module).vcd: $(toplevel_module)
	./$(toplevel_module) -V $(toplevel_module).vcd

$(BDIR) $(SDIR):
	mkdir -p $@

vcd_view:
	gtkwave $(toplevel_module).vcd &

clean:
	rm -rf $(BDIR) $(SDIR) *.so $(toplevel_module) *.vcd
