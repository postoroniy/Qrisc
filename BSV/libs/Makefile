toplevel_module = mkTop
default: all

export PATH:=${HOME}/projects/bluespec/bsc/inst/bin:$(PATH)
DISTRO ?= ${HOME}/projects/bluespec/bsc/inst/lib
BSC = bsc
BSC_PATHS = $(DISTRO)::%/Libraries:.

BSC_FLAGS = -u
BSC_VER_OPTS = -elab
BSC_SIM_OPTS = -sim

# BSC_COMPILATION_FLAGS ?= -keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps -check-assert \
# -suppress-warnings G0020 +RTS -K128M -RTS -show-range-conflict -u -elab -bdir $(BDIR)

BDIR ?= build
SDIR ?= sim

$(BDIR)/$(toplevel_module).ba: $(BDIR) $(toplevel_module).bsv mkMultiCycleFunction.bsv
	$(BSC) $(BSC_SIM_OPTS) $(BSC_FLAGS) -p $(BSC_PATHS) -bdir $(BDIR) -g $(toplevel_module) $(toplevel_module).bsv

$(toplevel_module): $(BDIR)/$(toplevel_module).ba $(SDIR)
	$(BSC) $(BSC_SIM_OPTS) -simdir $(SDIR) -e $(toplevel_module) -p $(BSC_PATHS) -bdir $(BDIR) -o $(toplevel_module)

all: $(toplevel_module).vcd

#$(toplevel_module).vcd: $(toplevel_module) random_bdpi.so
$(toplevel_module).vcd: $(toplevel_module)
	./$(toplevel_module) -V $(toplevel_module).vcd +seed=100
#LD_PRELOAD=./random_bdpi.so ./$(toplevel_module) -V $(toplevel_module).vcd +seed=100

random_bdpi.so: random_bdpi.c
	gcc -fPIC -shared -o random_bdpi.so random_bdpi.c

$(BDIR) $(SDIR):
	mkdir -p $@

test:
	$(BSC) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS):$(BDIR) -bdir $(BDIR) \
	-remove-dollar -g mkTop -verilog mkTop.bsv
# need to add clk and reset drivers
#iverilog -o simulation  mkTop.v
#vvp -i ./simulation -fst

vcd_view:
	gtkwave $(toplevel_module).vcd &

clean:
	rm -rf $(BDIR) $(SDIR) *.so $(toplevel_module) *.vcd
