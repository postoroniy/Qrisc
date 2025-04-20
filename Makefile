.PHONY: all test axi \
	sv-smoke sv-flags sv-isa sv-axi \
	bsv-smoke bsv-flags bsv-isa bsv-axi \
	amaranth amaranth-venv amaranth-smoke amaranth-flags amaranth-isa amaranth-axi \
	clash clash-install clash-version clash-smoke clash-flags clash-isa clash-axi \
	bsc bsc-smoke bsc-flags bsc-isa bsc-axi \
	synth-generic sv-synth bsv-synth clean

all: test

test: sv-smoke sv-flags sv-isa sv-axi \
	bsv-smoke bsv-flags bsv-isa bsv-axi \
	amaranth-smoke amaranth-flags amaranth-isa amaranth-axi \
	clash-smoke clash-flags clash-isa clash-axi \
	bsc-smoke bsc-flags bsc-isa bsc-axi

axi: sv-axi bsv-axi amaranth-axi clash-axi bsc-axi

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

synth-generic: sv-synth bsv-synth

sv-synth:
	$(MAKE) -C SystemVerilog synth

bsv-synth:
	$(MAKE) -C BSV synth

clean:
	$(MAKE) -C SystemVerilog clean
	$(MAKE) -C BSV clean
	$(MAKE) -C Amaranth clean
	$(MAKE) -C Clash clean
	$(MAKE) -C BSC clean
