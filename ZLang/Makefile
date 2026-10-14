ZLANG_PYTHON ?= $(if $(ZLANG_ROOT),$(ZLANG_ROOT)/.venv/bin/python,python3)
ZLANG_BIN ?= $(if $(ZLANG_ROOT),$(ZLANG_ROOT)/.venv/bin/zlang,zlang)
EXPECTED_ZLANG_VERSION ?= 0.1.0a23

.PHONY: all test smoke flags isa axi hazards legacy-sorts version check verilog lint formal clean

all: test
test: smoke flags isa axi hazards

legacy-sorts:
	PYTHONPATH=../tools $(ZLANG_PYTHON) -m qrisc32.run_binary_compat ZLang --group sort

version:
	@actual="$$($(ZLANG_BIN) --version)"; \
		test "$$actual" = "zlang $(EXPECTED_ZLANG_VERSION)" || { \
			echo "FAIL: expected zlang $(EXPECTED_ZLANG_VERSION), got '$$actual'"; exit 1; }; \
		echo "PASS: $$actual"

smoke flags isa hazards:
	$(ZLANG_PYTHON) tb/run_vectors.py $@
	test -s build/vcd/$@.vcd

axi:
	$(ZLANG_PYTHON) tb/run_axi.py
	test -s build/vcd/axi.vcd

check:
	$(ZLANG_BIN) src/Qrisc32.zhl --top Qrisc32 --check
	$(ZLANG_BIN) src/Qrisc32.zhl --top Qrisc32Axi --check

verilog:
	mkdir -p build/verilog
	$(ZLANG_BIN) src/Qrisc32.zhl --top Qrisc32 --systemverilog build/verilog/Qrisc32.sv
	$(ZLANG_BIN) src/Qrisc32.zhl --top Qrisc32Axi --systemverilog build/verilog/Qrisc32Axi.sv

formal:
	mkdir -p build/formal
	$(ZLANG_BIN) src/Qrisc32.zhl --top Qrisc32 --verify --verification-report build/formal/core.json --verification-format json
	$(ZLANG_BIN) src/Qrisc32.zhl --top Qrisc32Axi --verify --verification-report build/formal/axi.json --verification-format json

lint: check verilog
	verible-verilog-format --inplace build/verilog/Qrisc32.sv build/verilog/Qrisc32Axi.sv
	verilator --lint-only -Wall -Wno-DECLFILENAME build/verilog/Qrisc32.sv --top-module Qrisc32
	verilator --lint-only -Wall -Wno-DECLFILENAME -Wno-UNUSEDSIGNAL build/verilog/Qrisc32Axi.sv --top-module Qrisc32Axi

clean:
	rm -rf build
