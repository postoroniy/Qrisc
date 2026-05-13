//////////////////////////////////////////////////////////////////////////////////////////////
// AXI protocol smoke test for the SystemVerilog qrisc32 top.
//////////////////////////////////////////////////////////////////////////////////////////////
`timescale 1ns / 1ns
`default_nettype none

module qrisc32_axi_tb(
  input   logic   clk,
  input   logic   areset,
  output  logic   Istop_active,
  output  logic   Dstop_active,
  input   bit     Istop_enable,
  input   bit     Dstop_enable,
  input   bit     verbose,
  output  bit     done = 0
);

  localparam int IMEM_SIZE = 256;
  localparam int IMEM_ADR_LIMIT = 128;
  localparam int DMEM_SIZE = 256;
  localparam int DMEM_ADR_LIMIT = 256;

`include "axi_checked_tb_connect.svh"

  initial begin : init_mem
    integer i;
    for (i = 0; i < 256; i = i + 1) begin
      Iram.sram[i] = 32'h00000000;
      Dram.sram[i] = 32'h00000000;
    end

    $readmemh("../tests/axi_smoke.hex", Iram.sram);
  end

  logic [31:0] cycles;
  always_ff @(posedge clk) begin
    if (areset) begin
      cycles <= 32'd0;
      done <= 1'b0;
    end else begin
      cycles <= cycles + 1;
      if (cycles == 32'd900) begin
        if (Dram.sram[0] == 32'd12) begin
          $display("PASS: AXI smoke dmem[0] = %0d", Dram.sram[0]);
        end else begin
          $display("FAIL: AXI smoke dmem[0] = %0d (expected 12)", Dram.sram[0]);
          $fatal(1);
        end
        done <= 1'b1;
      end
    end
  end

endmodule

`default_nettype wire
