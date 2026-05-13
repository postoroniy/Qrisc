VENV ?= ../.venv
PYTHON ?= $(if $(wildcard $(VENV)/bin/python),$(VENV)/bin/python,python3)
PIP_CACHE_DIR ?= $(VENV)/pip-cache
PYTHONPATH := src
PYTEST = PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=$(PYTHONPATH) $(PYTHON) -m pytest
PYTEST_FLAGS ?= -q
PYTEST_XDIST ?= -n auto
BUILD_DIR ?= build
VERILOG_DIR ?= $(BUILD_DIR)/verilog
VERILOG_OUT ?= $(VERILOG_DIR)/qrisc32.v
VCD_DIR ?= $(BUILD_DIR)/vcd
VERILATOR_DIR ?= $(BUILD_DIR)/verilator
SV_RTL_DIR ?= ../SystemVerilog/rtl
SV_TB_DIR ?= ../SystemVerilog/tb
SV_CPP_DIR ?= ../SystemVerilog/cpp
VERILATOR_CFLAGS ?= -O0 -x-assign 0 --trace -Wno-fatal -Wno-lint -Wno-UNOPTFLAT -I$(SV_RTL_DIR) -I$(SV_TB_DIR)
VERILATOR_LINT_FLAGS ?= --lint-only -Wall
LINT_WAIVERS ?= lint/verilator.vlt
PY_SOURCES = src/Qrisc32Model/__init__.py src/Qrisc32Model/model.py src/Qrisc32Model/runner.py src/Qrisc32Model/rtl.py src/Qrisc32Model/verilog.py tb/conftest.py tb/test_vectors.py
VERILATOR_THREADS ?= 2
VERILATOR_ENV ?= CCACHE_DISABLE=1

SMOKE_BUILD=$(VERILATOR_DIR)/smoke
FLAGS_BUILD=$(VERILATOR_DIR)/flags
ISA_BUILD=$(VERILATOR_DIR)/isa
AXI_BUILD=$(VERILATOR_DIR)/axi

SMOKE_SRC=$(SV_RTL_DIR)/Qrisc_pack.sv $(SV_TB_DIR)/axi_mem.sv $(VERILOG_OUT) $(SV_TB_DIR)/Qrisc32Tb.sv
FLAGS_SRC=$(SV_RTL_DIR)/Qrisc_pack.sv $(SV_TB_DIR)/axi_mem.sv $(VERILOG_OUT) $(SV_TB_DIR)/Qrisc32FlagsTb.sv
ISA_SRC=$(SV_RTL_DIR)/Qrisc_pack.sv $(SV_TB_DIR)/axi_mem.sv $(VERILOG_OUT) $(SV_TB_DIR)/Qrisc32IsaTb.sv
AXI_SRC=$(SV_TB_DIR)/axi_checked_mem.sv $(VERILOG_OUT) $(SV_TB_DIR)/Qrisc32AxiTb.sv

.PHONY: all smoke flags isa axi test model pytest verilog synth lint venv clean

all: test

$(VERILOG_DIR) $(VCD_DIR) $(SMOKE_BUILD) $(FLAGS_BUILD) $(ISA_BUILD) $(AXI_BUILD):
	mkdir -p $@

$(VERILOG_OUT): src/Qrisc32Model/rtl.py src/Qrisc32Model/verilog.py | $(VERILOG_DIR)
	PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=$(PYTHONPATH) $(PYTHON) -m Qrisc32Model.verilog $(VERILOG_OUT)

verilog: $(VERILOG_OUT)

synth: $(VERILOG_OUT) ys
	mkdir -p $(BUILD_DIR)/yosys
	yosys -s ys

$(SMOKE_BUILD)/Vqrisc32_tb: $(SMOKE_SRC) $(SV_CPP_DIR)/Qrisc32Tb.cpp | $(SMOKE_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(SMOKE_SRC) --Mdir $(SMOKE_BUILD) --threads $(VERILATOR_THREADS) --build $(SV_CPP_DIR)/Qrisc32Tb.cpp --top qrisc32_tb

smoke: $(SMOKE_BUILD)/Vqrisc32_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_tb.vcd
	./$(SMOKE_BUILD)/Vqrisc32_tb
	test -s $(VCD_DIR)/qrisc32_tb.vcd

$(FLAGS_BUILD)/Vqrisc32_flags_tb: $(FLAGS_SRC) $(SV_CPP_DIR)/Qrisc32FlagsTb.cpp | $(FLAGS_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(FLAGS_SRC) --Mdir $(FLAGS_BUILD) --threads $(VERILATOR_THREADS) --build $(SV_CPP_DIR)/Qrisc32FlagsTb.cpp --top qrisc32_flags_tb

flags: $(FLAGS_BUILD)/Vqrisc32_flags_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_flags_tb.vcd
	./$(FLAGS_BUILD)/Vqrisc32_flags_tb
	test -s $(VCD_DIR)/qrisc32_flags_tb.vcd

$(ISA_BUILD)/Vqrisc32_isa_tb: $(ISA_SRC) $(SV_CPP_DIR)/Qrisc32IsaTb.cpp | $(ISA_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(ISA_SRC) --Mdir $(ISA_BUILD) --threads $(VERILATOR_THREADS) --build $(SV_CPP_DIR)/Qrisc32IsaTb.cpp --top qrisc32_isa_tb

isa: $(ISA_BUILD)/Vqrisc32_isa_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_isa_tb.vcd
	./$(ISA_BUILD)/Vqrisc32_isa_tb
	test -s $(VCD_DIR)/qrisc32_isa_tb.vcd

$(AXI_BUILD)/Vqrisc32_axi_tb: $(AXI_SRC) $(SV_CPP_DIR)/Qrisc32AxiTb.cpp | $(AXI_BUILD)
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $(AXI_SRC) --Mdir $(AXI_BUILD) --threads $(VERILATOR_THREADS) --build $(SV_CPP_DIR)/Qrisc32AxiTb.cpp --top qrisc32_axi_tb

axi: $(AXI_BUILD)/Vqrisc32_axi_tb | $(VCD_DIR)
	rm -f $(VCD_DIR)/qrisc32_axi_tb.vcd
	./$(AXI_BUILD)/Vqrisc32_axi_tb
	test -s $(VCD_DIR)/qrisc32_axi_tb.vcd

test:
	$(MAKE) smoke flags isa axi

model pytest:
	$(PYTEST) $(PYTEST_FLAGS) $(PYTEST_XDIST) tb --vector all

lint: $(VERILOG_OUT) $(LINT_WAIVERS)
	PYTHONDONTWRITEBYTECODE=1 $(PYTHON) -m py_compile $(PY_SOURCES)
	$(PYTEST) $(PYTEST_FLAGS) $(PYTEST_XDIST) tb --vector all
	$(VERILATOR_ENV) verilator $(VERILATOR_LINT_FLAGS) $(LINT_WAIVERS) $(VERILOG_OUT) --top-module qrisc32

venv:
	python3 -m venv $(VENV)
	PIP_CACHE_DIR=$(PIP_CACHE_DIR) $(VENV)/bin/python -m pip install -e .

clean:
	rm -rf build .pytest_cache src/*.egg-info
