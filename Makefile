.PHONY: all test axi \
	sv-smoke sv-flags sv-isa sv-axi sv-hazards \
	bsv-smoke bsv-flags bsv-isa bsv-axi bsv-hazards \
	amaranth amaranth-venv amaranth-smoke amaranth-flags amaranth-isa amaranth-axi amaranth-hazards \
	clash clash-install clash-version clash-smoke clash-flags clash-isa clash-axi clash-hazards \
	bsc bsc-smoke bsc-flags bsc-isa bsc-axi bsc-hazards \
	hazards \
	asm-tools asm-test c-test toolchain-test \
	lint sv-lint bsv-lint amaranth-lint clash-lint bsc-lint \
	synth-generic sv-synth bsv-synth amaranth-synth clash-synth bsc-synth clean

all: test

test: sv-smoke sv-flags sv-isa sv-axi \
	bsv-smoke bsv-flags bsv-isa bsv-axi \
	amaranth-smoke amaranth-flags amaranth-isa amaranth-axi \
	clash-smoke clash-flags clash-isa clash-axi \
	bsc-smoke bsc-flags bsc-isa bsc-axi \
	hazards

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

asm-tools:
	$(MAKE) -C tools asm-tools

asm-test:
	$(MAKE) -C tools asm-test

c-test:
	$(MAKE) -C tools c-test

toolchain-test:
	$(MAKE) -C tools toolchain-test

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

clean:
	$(MAKE) -C SystemVerilog clean
	$(MAKE) -C BSV clean
	$(MAKE) -C Amaranth clean
	$(MAKE) -C Clash clean
	$(MAKE) -C BSC clean
	$(MAKE) -C tools clean
