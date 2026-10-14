PYTHON ?= python3

.PHONY: all test axi \
	sv-smoke sv-flags sv-isa sv-axi sv-hazards \
	bsv-smoke bsv-flags bsv-isa bsv-axi bsv-hazards \
	amaranth amaranth-venv amaranth-smoke amaranth-flags amaranth-isa amaranth-axi amaranth-hazards \
	clash clash-install clash-version clash-smoke clash-flags clash-isa clash-axi clash-hazards \
	bsc bsc-smoke bsc-flags bsc-isa bsc-axi bsc-hazards \
	zlang zlang-smoke zlang-flags zlang-isa zlang-axi zlang-hazards zlang-version zlang-check zlang-verilog zlang-lint \
	hazards \
	asm-tools asm-test c-test toolchain-test \
	binary-compat sv-binary bsv-binary amaranth-binary clash-binary bsc-binary zlang-binary \
	sort-test sort-binary-compat legacy-sorts update-smoke-golden \
	lint sv-lint bsv-lint amaranth-lint clash-lint bsc-lint \
	synth-generic sv-synth bsv-synth amaranth-synth clash-synth bsc-synth \
	release-audit release-check clean

all: test

test: sv-smoke sv-flags sv-isa sv-axi \
	bsv-smoke bsv-flags bsv-isa bsv-axi \
	amaranth-smoke amaranth-flags amaranth-isa amaranth-axi \
	clash-smoke clash-flags clash-isa clash-axi \
	bsc-smoke bsc-flags bsc-isa bsc-axi \
	hazards zlang binary-compat

axi: sv-axi bsv-axi amaranth-axi clash-axi bsc-axi
hazards: sv-hazards bsv-hazards amaranth-hazards clash-hazards bsc-hazards

lint: sv-lint bsv-lint amaranth-lint clash-lint bsc-lint

sv-smoke:
	$(MAKE) -C SystemVerilog smoke

sv-flags:
	$(MAKE) -C SystemVerilog flags

sv-isa:
	$(MAKE) -C SystemVerilog isa

sv-axi:
	$(MAKE) -C SystemVerilog axi

sv-hazards:
	$(MAKE) -C SystemVerilog hazards

bsv-smoke:
	$(MAKE) -C BSV smoke

bsv-flags:
	$(MAKE) -C BSV flags

bsv-isa:
	$(MAKE) -C BSV isa

bsv-axi:
	$(MAKE) -C BSV axi

bsv-hazards:
	$(MAKE) -C BSV hazards

amaranth:
	$(MAKE) -C Amaranth test

amaranth-venv:
	$(MAKE) -C Amaranth venv

amaranth-smoke:
	$(MAKE) -C Amaranth smoke

amaranth-flags:
	$(MAKE) -C Amaranth flags

amaranth-isa:
	$(MAKE) -C Amaranth isa

amaranth-axi:
	$(MAKE) -C Amaranth axi

amaranth-hazards:
	$(MAKE) -C Amaranth hazards

clash: clash-smoke clash-flags clash-isa clash-axi clash-hazards

clash-install:
	$(MAKE) -C Clash install-clash

clash-version:
	$(MAKE) -C Clash clash-version

clash-smoke:
	$(MAKE) -C Clash smoke

clash-flags:
	$(MAKE) -C Clash flags

clash-isa:
	$(MAKE) -C Clash isa

clash-axi:
	$(MAKE) -C Clash axi

clash-hazards:
	$(MAKE) -C Clash hazards

bsc: bsc-smoke bsc-flags bsc-isa bsc-axi bsc-hazards

bsc-smoke:
	$(MAKE) -C BSC smoke

bsc-flags:
	$(MAKE) -C BSC flags

bsc-isa:
	$(MAKE) -C BSC isa

bsc-axi:
	$(MAKE) -C BSC axi

bsc-hazards:
	$(MAKE) -C BSC hazards

zlang: zlang-smoke zlang-flags zlang-isa zlang-axi zlang-hazards

zlang-smoke:
	$(MAKE) -C ZLang smoke

zlang-flags:
	$(MAKE) -C ZLang flags

zlang-isa:
	$(MAKE) -C ZLang isa

zlang-axi:
	$(MAKE) -C ZLang axi

zlang-hazards:
	$(MAKE) -C ZLang hazards

zlang-version:
	$(MAKE) -C ZLang version

zlang-check:
	$(MAKE) -C ZLang check

zlang-verilog:
	$(MAKE) -C ZLang verilog

zlang-lint:
	$(MAKE) -C ZLang lint

asm-tools:
	$(MAKE) -C tools asm-tools

asm-test:
	$(MAKE) -C tools asm-test

c-test:
	$(MAKE) -C tools c-test

toolchain-test:
	$(MAKE) -C tools toolchain-test

sort-test:
	$(MAKE) -C tools sort-test

sort-binary-compat: sort-test
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat BSV --group sort
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat Amaranth --group sort
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat Clash --group sort
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat BSC --group sort
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat ZLang --group sort

legacy-sorts: sort-binary-compat

update-smoke-golden:
	$(MAKE) -C tools update-smoke-golden

binary-compat: sv-binary bsv-binary amaranth-binary clash-binary bsc-binary zlang-binary

sv-binary:
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat SystemVerilog

bsv-binary:
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat BSV

amaranth-binary:
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat Amaranth

clash-binary:
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat Clash

bsc-binary:
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat BSC

zlang-binary:
	PYTHONPATH=tools $(PYTHON) -m qrisc32.run_binary_compat ZLang

sv-lint:
	$(MAKE) -C SystemVerilog lint

bsv-lint:
	$(MAKE) -C BSV lint

amaranth-lint:
	$(MAKE) -C Amaranth lint

clash-lint:
	$(MAKE) -C Clash lint

bsc-lint:
	$(MAKE) -C BSC lint

synth-generic: sv-synth bsv-synth amaranth-synth clash-synth bsc-synth

sv-synth:
	$(MAKE) -C SystemVerilog synth

bsv-synth:
	$(MAKE) -C BSV synth

amaranth-synth:
	$(MAKE) -C Amaranth synth

clash-synth:
	$(MAKE) -C Clash synth

bsc-synth:
	$(MAKE) -C BSC synth

release-audit:
	$(PYTHON) tools/release_audit.py
	@test "$$(tr -d '\n' < VERSION)" = "$$(sed -n 's/^version = "\(.*\)"/\1/p' ZLang/zlang.toml)" || { \
		echo "FAIL: VERSION and ZLang/zlang.toml disagree"; exit 1; }
	@echo "PASS: release version $$(cat VERSION)"

release-check: release-audit zlang-version test lint toolchain-test synth-generic zlang-lint
	$(MAKE) -C doc pdf
	test -s doc/build/risc_present/risc_present.pdf
	git diff --check

clean:
	$(MAKE) -C SystemVerilog clean
	$(MAKE) -C BSV clean
	$(MAKE) -C Amaranth clean
	$(MAKE) -C Clash clean
	$(MAKE) -C BSC clean
	$(MAKE) -C ZLang clean
	$(MAKE) -C tools clean
