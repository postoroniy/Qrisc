#!/usr/bin/env python3
import argparse
import json
from pathlib import Path


def sv_string(text):
    return text.replace("\\", "\\\\").replace('"', '\\"')


def cpp_for(vector, top, vcd_path, max_ticks):
    cls = "V" + top
    return f"""#include "{cls}.h"
#include <verilated.h>
#if VM_TRACE
#include "verilated_vcd_c.h"
#endif

int main(int argc, char **argv)
{{
  VerilatedContext *contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);
  {cls} *top = new {cls}{{contextp}};

#if VM_TRACE
  Verilated::traceEverOn(true);
  VerilatedVcdC *tfp = new VerilatedVcdC;
  top->trace(tfp, 99);
  tfp->open("{vcd_path}");
#endif

  top->clk = 0;
  top->areset = 1;
  top->verbose = 0;

  while (!contextp->gotFinish() && !top->done && contextp->time() < {max_ticks}) {{
    contextp->timeInc(1);
    top->clk = !top->clk;
    top->eval();
#if VM_TRACE
    tfp->dump(contextp->time());
#endif
    if (contextp->time() >= 8) {{
      top->areset = 0;
    }}
  }}

#if VM_TRACE
  tfp->close();
#endif

  int rc = top->done ? 0 : 1;
  delete top;
  delete contextp;
  return rc;
}}
"""


def count_hex_words(path):
    return sum(1 for line in path.read_text().splitlines() if line.strip())


def padded_size(needed, minimum):
    size = minimum
    while size < needed:
        size *= 2
    return size


def sv_for(spec, tests_dir):
    name = spec["name"]
    top = f"qrisc32_{name}_tb"
    expected = {int(k): int(v) for k, v in spec["expected_dmem"].items()}
    expected_expr = " && ".join(
        f"(Dram.sram[{idx}] === 32'd{value})" for idx, value in sorted(expected.items())
    )
    if not expected_expr:
        expected_expr = "1'b1"

    fail_checks = "\n".join(
        f"""          if (Dram.sram[{idx}] !== 32'd{value})
            $display("FAIL: dmem[{idx}]=%0d expected {value}", Dram.sram[{idx}]);"""
        for idx, value in sorted(expected.items())
    )

    dmem_init = ""
    if spec.get("dmem_init"):
        dmem_init = f'    $readmemh("../tests/{spec["dmem_init"]}", Dram.sram);\n'

    imem_words = count_hex_words(tests_dir / spec["hex"])
    dmem_init_words = count_hex_words(tests_dir / spec["dmem_init"]) if spec.get("dmem_init") else 0
    dmem_expected_words = max(expected.keys(), default=0) + 1
    imem_size = padded_size(imem_words + 16, 256)
    dmem_size = padded_size(max(dmem_init_words, dmem_expected_words) + 16, 256)

    max_cycles = 500_000 if name == "smoke" else 50_000
    pass_line = sv_string(spec["pass"])
    hex_file = spec["hex"]

    return top, max_cycles, f"""`timescale 1ns / 1ns
`default_nettype none

module {top}(
  input  logic clk,
  input  logic areset,
  input  bit   verbose,
  output bit   done = 0
);

  localparam int IMEM_SIZE = {imem_size};
  localparam int IMEM_ADR_LIMIT = {imem_size - 1};
  localparam int DMEM_SIZE = {dmem_size};
  localparam int DMEM_ADR_LIMIT = {dmem_size - 1};
  localparam int MAX_CYCLES = {max_cycles};

  logic [0:0]  axi_instruction_arid;
  logic [31:0] axi_instruction_araddr;
  logic [7:0]  axi_instruction_arlen;
  logic [2:0]  axi_instruction_arsize;
  logic [1:0]  axi_instruction_arburst;
  logic        axi_instruction_arlock;
  logic [3:0]  axi_instruction_arcache;
  logic [2:0]  axi_instruction_arprot;
  logic [3:0]  axi_instruction_arqos;
  logic [3:0]  axi_instruction_arregion;
  logic [0:0]  axi_instruction_aruser;
  logic        axi_instruction_arvalid;
  logic        axi_instruction_arready;
  logic [0:0]  axi_instruction_rid;
  logic [31:0] axi_instruction_rdata;
  logic [1:0]  axi_instruction_rresp;
  logic        axi_instruction_rlast;
  logic [0:0]  axi_instruction_ruser;
  logic        axi_instruction_rvalid;
  logic        axi_instruction_rready;

  logic [0:0]  axi_data_arid;
  logic [31:0] axi_data_araddr;
  logic [7:0]  axi_data_arlen;
  logic [2:0]  axi_data_arsize;
  logic [1:0]  axi_data_arburst;
  logic        axi_data_arlock;
  logic [3:0]  axi_data_arcache;
  logic [2:0]  axi_data_arprot;
  logic [3:0]  axi_data_arqos;
  logic [3:0]  axi_data_arregion;
  logic [0:0]  axi_data_aruser;
  logic        axi_data_arvalid;
  logic        axi_data_arready;
  logic [0:0]  axi_data_rid;
  logic [31:0] axi_data_rdata;
  logic [1:0]  axi_data_rresp;
  logic        axi_data_rlast;
  logic [0:0]  axi_data_ruser;
  logic        axi_data_rvalid;
  logic        axi_data_rready;
  logic [0:0]  axi_data_awid;
  logic [31:0] axi_data_awaddr;
  logic [7:0]  axi_data_awlen;
  logic [2:0]  axi_data_awsize;
  logic [1:0]  axi_data_awburst;
  logic        axi_data_awlock;
  logic [3:0]  axi_data_awcache;
  logic [2:0]  axi_data_awprot;
  logic [3:0]  axi_data_awqos;
  logic [3:0]  axi_data_awregion;
  logic [0:0]  axi_data_awuser;
  logic        axi_data_awvalid;
  logic        axi_data_awready;
  logic [31:0] axi_data_wdata;
  logic [3:0]  axi_data_wstrb;
  logic        axi_data_wlast;
  logic [0:0]  axi_data_wuser;
  logic        axi_data_wvalid;
  logic        axi_data_wready;
  logic [0:0]  axi_data_bid;
  logic [1:0]  axi_data_bresp;
  logic [0:0]  axi_data_buser;
  logic        axi_data_bvalid;
  logic        axi_data_bready;

  bit Istop_active;
  bit Dstop_active;

  axi_checked_read_mem #(
    .size(IMEM_SIZE),
    .adr_limit(IMEM_ADR_LIMIT),
    .ready_phase(0)
  ) Iram (
    .clk(clk),
    .areset(areset),
    .arid(axi_instruction_arid),
    .araddr(axi_instruction_araddr),
    .arlen(axi_instruction_arlen),
    .arsize(axi_instruction_arsize),
    .arburst(axi_instruction_arburst),
    .arlock(axi_instruction_arlock),
    .arcache(axi_instruction_arcache),
    .arprot(axi_instruction_arprot),
    .arqos(axi_instruction_arqos),
    .arregion(axi_instruction_arregion),
    .aruser(axi_instruction_aruser),
    .arvalid(axi_instruction_arvalid),
    .arready(axi_instruction_arready),
    .rid(axi_instruction_rid),
    .rdata(axi_instruction_rdata),
    .rresp(axi_instruction_rresp),
    .rlast(axi_instruction_rlast),
    .ruser(axi_instruction_ruser),
    .rvalid(axi_instruction_rvalid),
    .rready(axi_instruction_rready),
    .stop_enable(1'b1),
    .stop_active(Istop_active),
    .verbose(verbose)
  );

  axi_checked_rw_mem #(
    .size(DMEM_SIZE),
    .adr_limit(DMEM_ADR_LIMIT),
    .read_ready_phase(2),
    .aw_ready_phase(1),
    .w_ready_phase(3)
  ) Dram (
    .clk(clk),
    .areset(areset),
    .arid(axi_data_arid),
    .araddr(axi_data_araddr),
    .arlen(axi_data_arlen),
    .arsize(axi_data_arsize),
    .arburst(axi_data_arburst),
    .arlock(axi_data_arlock),
    .arcache(axi_data_arcache),
    .arprot(axi_data_arprot),
    .arqos(axi_data_arqos),
    .arregion(axi_data_arregion),
    .aruser(axi_data_aruser),
    .arvalid(axi_data_arvalid),
    .arready(axi_data_arready),
    .rid(axi_data_rid),
    .rdata(axi_data_rdata),
    .rresp(axi_data_rresp),
    .rlast(axi_data_rlast),
    .ruser(axi_data_ruser),
    .rvalid(axi_data_rvalid),
    .rready(axi_data_rready),
    .awid(axi_data_awid),
    .awaddr(axi_data_awaddr),
    .awlen(axi_data_awlen),
    .awsize(axi_data_awsize),
    .awburst(axi_data_awburst),
    .awlock(axi_data_awlock),
    .awcache(axi_data_awcache),
    .awprot(axi_data_awprot),
    .awqos(axi_data_awqos),
    .awregion(axi_data_awregion),
    .awuser(axi_data_awuser),
    .awvalid(axi_data_awvalid),
    .awready(axi_data_awready),
    .wdata(axi_data_wdata),
    .wstrb(axi_data_wstrb),
    .wlast(axi_data_wlast),
    .wuser(axi_data_wuser),
    .wvalid(axi_data_wvalid),
    .wready(axi_data_wready),
    .bid(axi_data_bid),
    .bresp(axi_data_bresp),
    .buser(axi_data_buser),
    .bvalid(axi_data_bvalid),
    .bready(axi_data_bready),
    .stop_enable(1'b1),
    .stop_active(Dstop_active),
    .verbose(verbose)
  );

  qrisc32 dut (
    .clk(clk),
    .areset(areset),
    .axi_instruction_arready(axi_instruction_arready),
    .axi_instruction_rid(axi_instruction_rid),
    .axi_instruction_rdata(axi_instruction_rdata),
    .axi_instruction_rresp(axi_instruction_rresp),
    .axi_instruction_rlast(axi_instruction_rlast),
    .axi_instruction_ruser(axi_instruction_ruser),
    .axi_instruction_rvalid(axi_instruction_rvalid),
    .axi_data_arready(axi_data_arready),
    .axi_data_rid(axi_data_rid),
    .axi_data_rdata(axi_data_rdata),
    .axi_data_rresp(axi_data_rresp),
    .axi_data_rlast(axi_data_rlast),
    .axi_data_ruser(axi_data_ruser),
    .axi_data_rvalid(axi_data_rvalid),
    .axi_data_awready(axi_data_awready),
    .axi_data_wready(axi_data_wready),
    .axi_data_bid(axi_data_bid),
    .axi_data_bresp(axi_data_bresp),
    .axi_data_buser(axi_data_buser),
    .axi_data_bvalid(axi_data_bvalid),
    .verbose(verbose),
    .axi_instruction_arid(axi_instruction_arid),
    .axi_instruction_araddr(axi_instruction_araddr),
    .axi_instruction_arlen(axi_instruction_arlen),
    .axi_instruction_arsize(axi_instruction_arsize),
    .axi_instruction_arburst(axi_instruction_arburst),
    .axi_instruction_arlock(axi_instruction_arlock),
    .axi_instruction_arcache(axi_instruction_arcache),
    .axi_instruction_arprot(axi_instruction_arprot),
    .axi_instruction_arqos(axi_instruction_arqos),
    .axi_instruction_arregion(axi_instruction_arregion),
    .axi_instruction_aruser(axi_instruction_aruser),
    .axi_instruction_arvalid(axi_instruction_arvalid),
    .axi_instruction_rready(axi_instruction_rready),
    .axi_data_arid(axi_data_arid),
    .axi_data_araddr(axi_data_araddr),
    .axi_data_arlen(axi_data_arlen),
    .axi_data_arsize(axi_data_arsize),
    .axi_data_arburst(axi_data_arburst),
    .axi_data_arlock(axi_data_arlock),
    .axi_data_arcache(axi_data_arcache),
    .axi_data_arprot(axi_data_arprot),
    .axi_data_arqos(axi_data_arqos),
    .axi_data_arregion(axi_data_arregion),
    .axi_data_aruser(axi_data_aruser),
    .axi_data_arvalid(axi_data_arvalid),
    .axi_data_rready(axi_data_rready),
    .axi_data_awid(axi_data_awid),
    .axi_data_awaddr(axi_data_awaddr),
    .axi_data_awlen(axi_data_awlen),
    .axi_data_awsize(axi_data_awsize),
    .axi_data_awburst(axi_data_awburst),
    .axi_data_awlock(axi_data_awlock),
    .axi_data_awcache(axi_data_awcache),
    .axi_data_awprot(axi_data_awprot),
    .axi_data_awqos(axi_data_awqos),
    .axi_data_awregion(axi_data_awregion),
    .axi_data_awuser(axi_data_awuser),
    .axi_data_awvalid(axi_data_awvalid),
    .axi_data_wdata(axi_data_wdata),
    .axi_data_wstrb(axi_data_wstrb),
    .axi_data_wlast(axi_data_wlast),
    .axi_data_wuser(axi_data_wuser),
    .axi_data_wvalid(axi_data_wvalid),
    .axi_data_bready(axi_data_bready)
  );

  initial begin : init_mem
    integer i;
    for (i = 0; i < IMEM_SIZE; i = i + 1) begin
      Iram.sram[i] = 32'h00000000;
    end
    for (i = 0; i < DMEM_SIZE; i = i + 1) begin
      Dram.sram[i] = 32'h00000000;
    end
    $readmemh("../tests/{hex_file}", Iram.sram);
{dmem_init}  end

  logic [31:0] cycles;
  always_ff @(posedge clk) begin
    if (areset) begin
      cycles <= 32'd0;
      done <= 1'b0;
    end else begin
      cycles <= cycles + 1;
      if (cycles > 32'd20 && ({expected_expr})) begin
        $display("{pass_line}");
        done <= 1'b1;
      end else if (cycles == MAX_CYCLES) begin
{fail_checks}
        $fatal(1);
      end
    end
  end

endmodule

`default_nettype wire
"""


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--vector", required=True)
    parser.add_argument("--manifest", default="../tests/vectors.json")
    parser.add_argument("--out-dir", required=True)
    parser.add_argument("--vcd-dir", default="build/vcd")
    args = parser.parse_args()

    manifest_path = Path(args.manifest)
    manifest = json.loads(manifest_path.read_text())
    tests_dir = manifest_path.parent
    specs = {entry["name"]: entry for entry in manifest["vectors"]}
    spec = specs[args.vector]

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    top, max_cycles, sv_text = sv_for(spec, tests_dir)
    (out_dir / f"{top}.sv").write_text(sv_text)
    (out_dir / f"{top}.cpp").write_text(
        cpp_for(args.vector, top, f"{args.vcd_dir}/{args.vector}.vcd", max_cycles * 2 + 100)
    )


if __name__ == "__main__":
    main()
