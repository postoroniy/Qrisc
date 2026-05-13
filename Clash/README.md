# qrisc32 in Clash

Status: five-stage Clash RTL implementation with generated-HDL smoke, flags,
ISA, and AXI smoke checks.

The SystemVerilog implementation is the canonical qrisc32 behavior.  Clash work
consumes the shared programs described by `../tests/vectors.json` and reproduces
the same architectural results through generated Verilog and Verilator.  The
RTL now stages instruction fetch, decode, execute, memory response handling, and
writeback explicitly; architectural register and flag commits happen in WB.

Source layout:

- `src/`: synthesizable qrisc32 RTL plus the retained architectural model.
- `tb/`: generated-HDL bench generator and model harness.
- `build/`: generated outputs only.

Local checks:

```sh
make -C Clash smoke flags isa axi
make -C Clash verilog
make -C Clash synth
make -C Clash lint
```

Model-only checks:

```sh
make -C Clash model
```

Local Clash compiler:

```sh
make clash-install
make clash-version
```

`make clash-install` builds `exe:clash` from the sibling checkout at
`../../../clash/clash-compiler` by default and copies the binary to
`./.local/bin/clash` in this repository. Override `CLASH_COMPILER_ROOT`,
`CLASH_GHC`, or `CLASH_BIN_DIR` if the checkout or compiler lives elsewhere.

The lint target runs GHC `-Wall -fno-code` for the RTL and model sources, then
Verilator lint on generated Clash HDL with scoped generated-code waivers.
