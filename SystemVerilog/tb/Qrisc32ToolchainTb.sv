//////////////////////////////////////////////////////////////////////////////////////////////
// Generic generated-program testbench for qrisc32 toolchain outputs.
//////////////////////////////////////////////////////////////////////////////////////////////
`timescale 1ns / 1ns
`default_nettype none

module qrisc32_toolchain_tb(
  input   logic   clk,
  input   logic   areset,
  output  logic   Istop_active,
  output  logic   Dstop_active,
  input   bit     Istop_enable,
  input   bit     Dstop_enable,
  input   bit     verbose,
  output  bit     done = 0
);

  localparam int IMEM_SIZE = 4096;
  localparam int IMEM_ADR_LIMIT = 4096;
  localparam int DMEM_SIZE = 4096;
  localparam int DMEM_ADR_LIMIT = 4096;

`include "axi_tb_connect.svh"

  string imem_path;
  string dmem_path;
  string test_name;
  int timeout_cycles;
  int expect_count;
  int expected [0:31];

  initial begin : init_mem
    integer i;
    integer value;
    string plusarg;
    for (i = 0; i < IMEM_SIZE; i = i + 1) begin
      Iram.sram[i] = 32'h00000000;
    end
    for (i = 0; i < DMEM_SIZE; i = i + 1) begin
      Dram.sram[i] = 32'h00000000;
    end

    if (!$value$plusargs("IMEM=%s", imem_path)) begin
      $display("FAIL: missing +IMEM=<path>");
      $fatal(1);
    end
    $readmemh(imem_path, Iram.sram);

    if ($value$plusargs("DMEM=%s", dmem_path)) begin
      $readmemh(dmem_path, Dram.sram);
    end

    if (!$value$plusargs("NAME=%s", test_name)) begin
      test_name = "toolchain";
    end
    if (!$value$plusargs("TIMEOUT=%d", timeout_cycles)) begin
      timeout_cycles = 10000;
    end
    if (!$value$plusargs("EXPECT_COUNT=%d", expect_count)) begin
      expect_count = 0;
    end
    if (expect_count > 32) begin
      $display("FAIL: EXPECT_COUNT=%0d exceeds 32", expect_count);
      $fatal(1);
    end
    for (i = 0; i < expect_count; i = i + 1) begin
      plusarg = $sformatf("EXPECT%0d=%%d", i);
      if (!$value$plusargs(plusarg, value)) begin
        $display("FAIL: missing +EXPECT%0d=<value>", i);
        $fatal(1);
      end
      expected[i] = value;
    end
  end

  logic [31:0] cycles;
  always_ff @(posedge clk) begin
    if (areset) begin
      cycles <= 32'd0;
      done <= 1'b0;
    end else begin
      int i;
      bit ok;
      cycles <= cycles + 1;
      if (cycles == timeout_cycles[31:0]) begin
        ok = 1'b1;
        for (i = 0; i < expect_count; i = i + 1) begin
          if (Dram.sram[i] !== expected[i][31:0]) begin
            ok = 1'b0;
            $display("FAIL: %s dmem[%0d]=%0d expected %0d", test_name, i, Dram.sram[i], expected[i]);
          end
        end
        if (ok) begin
          $display("PASS: %s", test_name);
        end else begin
          $fatal(1);
        end
        done <= 1'b1;
      end
    end
  end

endmodule

`default_nettype wire
