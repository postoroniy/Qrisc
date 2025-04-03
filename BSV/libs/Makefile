
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

$(BDIR)/$(toplevel_module).ba: $(BDIR)
	bsc $(BSC_SIM_OPTS) $(BSC_FLAGS) -p $(BSC_PATHS) -bdir $(BDIR) -g $(toplevel_module) $(toplevel_module).bsv

$(toplevel_module): $(BDIR)/$(toplevel_module).ba $(SDIR)
	bsc $(BSC_SIM_OPTS) -simdir $(SDIR) -e $(toplevel_module) -p $(BSC_PATHS) -bdir $(BDIR) -o $(toplevel_module)

$(toplevel_module).bsv: $(BDIR)/$(toplevel_module).ba

all: $(toplevel_module).vcd

$(toplevel_module).vcd: $(toplevel_module)
	./$(toplevel_module) -V $(toplevel_module).vcd

$(BDIR) $(SDIR):
	mkdir -p $@

vcd_view:
	gtkwave $(toplevel_module).vcd &


#--------------------------------------------------------------------
# Clean up
#--------------------------------------------------------------------
clean:
	rm -rf $(BDIR) $(SDIR) *.so $(toplevel_module) *.vcd
