# qrisc32 RTL Synthesis Comparison

Captured on 2026-05-15 with Yosys 0.64 and Vivado 2024.2:

```sh
make synth-generic
```

All implementations completed the generic Yosys flow.  The flow uses each
implementation's local `ys` script and targets `synth_xilinx -family xc7
-noiopad -noclkbuf`.  This is a generic FPGA synthesis comparison only; it is
not ASIC area.

The resource table below was collected from the generated
`build/yosys/yosys_qrisc32_syn.v` netlists with:

```sh
yosys -p 'read_verilog <impl>/build/yosys/yosys_qrisc32_syn.v; hierarchy -top qrisc32; stat -tech xilinx'
```

The maximum operating frequency estimate was collected by placing and routing
the Yosys netlists out-of-context in Vivado:

```sh
source ~/.Xilinx/Vivado/2024.2/settings64.sh
vivado -mode batch -nojournal -nolog -source tools/vivado_fmax.tcl \
  -tclargs <impl> <impl>/build/yosys/yosys_qrisc32_syn.v <clock-port> \
  build/vivado_fmax/<impl>
```

The timing flow uses part `xc7z020clg400-1`, a 10.000 ns clock constraint, and
reports `Fmax = 1000 / (10 ns - WNS)`.  The run is useful for relative RTL
comparison, but it is not board-level timing closure because top-level pins and
clock source placement are intentionally unconstrained in the OOC flow.

## RTL Approach Summary

| Implementation | RTL source shape | Source LOC | Synth input | Top port bits | Notes |
| --- | --- | ---: | --- | ---: | --- |
| SystemVerilog | Handwritten multi-module 5-stage RTL | 1697 | `SystemVerilog/rtl/*.sv` | 322 | Canonical implementation. |
| BSV | Bluespec SystemVerilog stage modules plus generated Verilog | 1080 | `BSV/build/verilog/qrisc32.v` | 432 | Multi-file BSV pipeline; generated interface style creates more top ports. |
| Amaranth | Python HDL RTL plus generated Verilog | 870 | `Amaranth/build/verilog/qrisc32.v` | 322 | Keeps a separate Python architectural model. |
| Clash | Clash/Haskell RTL plus generated Verilog | 801 | `Clash/build/hdl/Qrisc32RTL.topEntity/qrisc32.v` | 322 | Keeps a separate Haskell architectural model. |
| BSC | Bluespec Classic single RTL source plus generated Verilog | 381 | `BSC/build/verilog/mkQrisc32.v`, `BSC/build/verilog/qrisc32.v` | 432 | Most concise source form; generated wrapper exposes Bluespec-style ports. |

## Post-Synthesis Resource Summary

| Implementation | Result | Est. LCs | LUTs | FFs | DSP48E1 | CARRY4 | MUXF7 | MUXF8 | Cells | Wires | Wire bits | WNS @10ns | Fmax MHz |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| SystemVerilog | PASS | 4726 | 5503 | 1753 | 3 | 28 | 341 | 103 | 7732 | 4393 | 18424 | -2.061 | 82.9 |
| BSV | PASS | 4001 | 5098 | 2285 | 3 | 28 | 569 | 205 | 9503 | 4640 | 18920 | -1.393 | 87.8 |
| Amaranth | PASS | 4808 | 5695 | 1656 | 3 | 28 | 459 | 127 | 7969 | 3496 | 14378 | -3.105 | 76.3 |
| Clash | PASS | 3327 | 4034 | 1324 | 3 | 62 | 245 | 35 | 5712 | 1817 | 47483 | -5.932 | 62.8 |
| BSC | PASS | 3172 | 3671 | 1485 | 3 | 44 | 527 | 109 | 7077 | 3530 | 12028 | -6.169 | 61.8 |

## LUT Breakdown

| Implementation | LUT1 | LUT2 | LUT3 | LUT4 | LUT5 | LUT6 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| SystemVerilog | 56 | 721 | 1859 | 192 | 959 | 1716 |
| BSV | 273 | 824 | 621 | 281 | 1356 | 1743 |
| Amaranth | 107 | 780 | 1851 | 199 | 1224 | 1534 |
| Clash | 15 | 692 | 1463 | 286 | 214 | 1364 |
| BSC | 7 | 492 | 1243 | 537 | 110 | 1282 |

## Notes

- All RTL approaches now infer 3 `DSP48E1` cells for the multiply paths.
- BSC is currently the smallest by estimated LCs, while Clash is close and has
  the lowest FF count.
- BSV has the best OOC post-route Fmax in this run, despite higher FF and cell
  counts.
- BSV has the largest FF and total cell counts, largely from the generated
  Bluespec interface and scheduling structure.
- SystemVerilog and Amaranth are close in estimated LCs.  The SystemVerilog
  debug displays are protected with `synthesis translate_off`, so Vivado can
  elaborate a hardware-only netlist.
- Vivado timing reports and compact Fmax summaries are written under
  `build/vivado_fmax/<impl>/`.
- These numbers should be refreshed after RTL, package, or AXI interface
  changes by rerunning `make synth-generic`, the Vivado Fmax flow, and updating
  this table.
