RUNGHC ?= runghc
CLASH_COMPILER_ROOT ?= ../../../clash/clash-compiler
CLASH_GHC ?= $(HOME)/.ghcup/ghc/9.6.7/bin/ghc
CLASH_BIN_DIR ?= ../.local/bin
CLASH_PACKAGE_DB ?= $(HOME)/.cabal/store/ghc-9.6.7/package.db
CLASH ?= $(if $(wildcard $(CLASH_BIN_DIR)/clash),$(CLASH_BIN_DIR)/clash,clash)

RUN_MODEL = $(RUNGHC) -isrc tb/Main.hs
BUILD_DIR = build
HDL_DIR = $(BUILD_DIR)/hdl
HDL_STAMP = $(HDL_DIR)/.stamp
VCD_DIR = $(BUILD_DIR)/vcd
BENCH_DIR = $(BUILD_DIR)/tb
VERILATOR_DIR = $(BUILD_DIR)/verilator
AXI_MEM = ../SystemVerilog/tb/axi_checked_mem.sv
VERILATOR_CFLAGS ?= -O0 -x-assign 0 --trace -Wno-fatal -Wno-lint -Wno-UNOPTFLAT
VERILATOR_THREADS ?= 2
VERILATOR_ENV ?= CCACHE_DISABLE=1
LOCAL_CLASH_PROJECT := build/local-clash/cabal.project
LOCAL_CLASH_ROOT := $(abspath $(CLASH_COMPILER_ROOT))
LOCAL_CLASH_BINDIR := $(abspath $(CLASH_BIN_DIR))

.PHONY: all smoke flags isa axi test verilog \
	model model-smoke model-flags model-isa model-axi \
	install-clash clash-version clean FORCE
.SECONDARY: $(BENCH_DIR)/%/.stamp
.PRECIOUS: build/tb/%/.stamp

all: smoke flags isa axi

$(LOCAL_CLASH_PROJECT): FORCE
	mkdir -p $(dir $@)
	{ \
		printf 'packages:\n'; \
		printf '  %s/clash-ffi\n' '$(LOCAL_CLASH_ROOT)'; \
		printf '  %s/clash-ghc\n' '$(LOCAL_CLASH_ROOT)'; \
		printf '  %s/clash-lib\n' '$(LOCAL_CLASH_ROOT)'; \
		printf '  %s/clash-prelude\n' '$(LOCAL_CLASH_ROOT)'; \
		printf '\nwrite-ghc-environment-files: never\n'; \
		printf 'index-state: 2026-05-09T11:46:50Z\n\n'; \
		printf 'package clash-ghc\n'; \
		printf '  ghc-options: +RTS -qn4 -A128M -RTS -j4\n'; \
		printf '  executable-dynamic: True\n\n'; \
		printf 'package clash-prelude\n'; \
		printf '  ghc-options: +RTS -qn4 -A128M -RTS -j4\n\n'; \
		printf 'package clash-lib\n'; \
		printf '  ghc-options: +RTS -qn4 -A128M -RTS -j4\n'; \
		printf '  flags: +debug\n\n'; \
		printf 'allow-newer:\n'; \
		printf '  brick:base,\n'; \
		printf '  brick:deepseq,\n'; \
		printf '  hashable,\n'; \
		printf '  hedgehog-fakedata:hedgehog,\n'; \
		printf '  rewrite-inspector:binary,\n'; \
		printf '  rewrite-inspector:containers,\n'; \
		printf '  vty:deepseq,\n'; \
		printf '  derive-storable-plugin:ghc,\n'; \
		printf '  derive-storable-plugin:ghci,\n'; \
		printf '  string-random:text,\n'; \
		printf '  string-random:containers,\n'; \
		printf '  string-interpolate:template-haskell,\n'; \
		printf '  string-interpolate:text,\n'; \
		printf '  hint:ghc\n\n'; \
		printf 'package recursion-schemes\n'; \
		printf '  optimization: 0\n\n'; \
		printf 'package regex-tdfa\n'; \
		printf '  optimization: 2\n\n'; \
		printf 'constraints: vty < 6.0\n'; \
	} > $@

install-clash: $(LOCAL_CLASH_PROJECT)
	mkdir -p $(LOCAL_CLASH_BINDIR)
	cabal v2-install exe:clash --project-file=$(LOCAL_CLASH_PROJECT) --with-compiler=$(CLASH_GHC) --builddir=build/cabal --installdir=$(LOCAL_CLASH_BINDIR) --install-method=copy --overwrite-policy=always --disable-tests --disable-benchmarks -j2

clash-version:
	$(CLASH) --version

$(HDL_STAMP): src/Qrisc32RTL.hs
	mkdir -p $(HDL_DIR)
	$(CLASH) --verilog -package-db $(CLASH_PACKAGE_DB) -isrc -outputdir $(BUILD_DIR)/clash -fclash-hdldir $(HDL_DIR) -fclash-clear Qrisc32RTL
	touch $(HDL_STAMP)

verilog: $(HDL_STAMP)

$(BENCH_DIR)/%/.stamp: tb/gen_vector_bench.py ../tests/vectors.json
	mkdir -p $(BENCH_DIR)/$*
	python3 tb/gen_vector_bench.py --vector $* --manifest ../tests/vectors.json --out-dir $(BENCH_DIR)/$* --vcd-dir $(VCD_DIR)
	touch $@

$(VERILATOR_DIR)/%/.stamp: $(HDL_STAMP) $(BENCH_DIR)/%/.stamp $(AXI_MEM)
	mkdir -p $(VERILATOR_DIR)/$* $(VCD_DIR)
	HDL_FILES="$$(find $(HDL_DIR) -name '*.v' -o -name '*.sv' | sort)"; \
	$(VERILATOR_ENV) verilator --cc --exe $(VERILATOR_CFLAGS) $$HDL_FILES $(AXI_MEM) $(BENCH_DIR)/$*/qrisc32_clash_$*_tb.sv --Mdir $(VERILATOR_DIR)/$* --threads $(VERILATOR_THREADS) --build $(BENCH_DIR)/$*/qrisc32_clash_$*_tb.cpp --top qrisc32_clash_$*_tb
	touch $@

smoke: $(VERILATOR_DIR)/smoke/.stamp
	rm -f $(VCD_DIR)/smoke.vcd
	./$(VERILATOR_DIR)/smoke/Vqrisc32_clash_smoke_tb
	test -s $(VCD_DIR)/smoke.vcd

flags: $(VERILATOR_DIR)/flags/.stamp
	rm -f $(VCD_DIR)/flags.vcd
	./$(VERILATOR_DIR)/flags/Vqrisc32_clash_flags_tb
	test -s $(VCD_DIR)/flags.vcd

isa: $(VERILATOR_DIR)/isa/.stamp
	rm -f $(VCD_DIR)/isa.vcd
	./$(VERILATOR_DIR)/isa/Vqrisc32_clash_isa_tb
	test -s $(VCD_DIR)/isa.vcd

axi: $(VERILATOR_DIR)/axi/.stamp
	rm -f $(VCD_DIR)/axi.vcd
	./$(VERILATOR_DIR)/axi/Vqrisc32_clash_axi_tb
	test -s $(VCD_DIR)/axi.vcd

test: all

model: model-smoke model-flags model-isa model-axi

model-smoke:
	$(RUN_MODEL) smoke

model-flags:
	$(RUN_MODEL) flags

model-isa:
	$(RUN_MODEL) isa

model-axi:
	$(RUN_MODEL) axi

clean:
	rm -rf build dist-newstyle
