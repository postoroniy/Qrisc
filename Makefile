.PHONY: all test axi \
	sv-smoke sv-flags sv-isa sv-axi \
	bsv-smoke bsv-flags bsv-isa bsv-axi \
	amaranth amaranth-venv amaranth-smoke amaranth-flags amaranth-isa amaranth-axi \
	clash clash-install clash-version clash-smoke clash-flags clash-isa clash-axi \
	bsc bsc-smoke bsc-flags bsc-isa bsc-axi \
	lint sv-lint bsv-lint amaranth-lint clash-lint bsc-lint \
	synth-generic sv-synth bsv-synth amaranth-synth clash-synth bsc-synth clean

all: test

test: sv-smoke sv-flags sv-isa sv-axi \
	bsv-smoke bsv-flags bsv-isa bsv-axi \
	amaranth-smoke amaranth-flags amaranth-isa amaranth-axi \
	clash-smoke clash-flags clash-isa clash-axi \
	bsc-smoke bsc-flags bsc-isa bsc-axi

axi: sv-axi bsv-axi amaranth-axi clash-axi bsc-axi

lint: sv-lint bsv-lint amaranth-lint clash-lint bsc-lint

sv-smoke:
	$(MAKE) -C SystemVerilog smoke

sv-flags:
	$(MAKE) -C SystemVerilog flags

sv-isa:
	$(MAKE) -C SystemVerilog isa

sv-axi:
	$(MAKE) -C SystemVerilog axi

bsv-smoke:
	$(MAKE) -C BSV smoke

bsv-flags:
	$(MAKE) -C BSV flags

bsv-isa:
	$(MAKE) -C BSV isa

bsv-axi:
	$(MAKE) -C BSV axi

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

clash: clash-smoke clash-flags clash-isa clash-axi

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

bsc: bsc-smoke bsc-flags bsc-isa bsc-axi

bsc-smoke:
	$(MAKE) -C BSC smoke

bsc-flags:
	$(MAKE) -C BSC flags

bsc-isa:
	$(MAKE) -C BSC isa

bsc-axi:
	$(MAKE) -C BSC axi

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
