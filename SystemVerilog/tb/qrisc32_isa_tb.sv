//////////////////////////////////////////////////////////////////////////////////////////////
//    Project Qrisc32 is risc cpu implementation, purpose is studying
//    Digital System Design course at Kyoung Hee University during my PhD earning
//    Copyright (C) 2010-2025  Viacheslav Vinogradov
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Lesser General Public
//    License as published by the Free Software Foundation; either
//    version 2.1 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//////////////////////////////////////////////////////////////////////////////////////////////
`timescale 1ns / 1ns
`default_nettype none

module qrisc32_isa_tb(
  input   logic   clk,
  input   logic   areset,
  output  logic   Istop_active,
  output  logic   Dstop_active,
  input   bit     Istop_enable,
  input   bit     Dstop_enable,
  input   bit     verbose,
  output  bit     done = 0
);

  localparam int IMEM_SIZE = 512;
  localparam int IMEM_ADR_LIMIT = 512;
  localparam int DMEM_SIZE = 512;
  localparam int DMEM_ADR_LIMIT = 512;

`include "axi_tb_connect.svh"

  logic [31:0] expected [0:27];

  initial begin : init_mem
    integer i;
    for (i = 0; i < 512; i = i + 1) begin
      Iram.sram[i] = 32'h00000000;
      Dram.sram[i] = 32'h00000000;
    end

    $readmemh("../tests/isa.hex", Iram.sram);

    expected[0]  = 32'd0;
    expected[1]  = 32'd1;
    expected[2]  = 32'd0;
    expected[3]  = 32'd0;
    expected[4]  = 32'd0;
    expected[5]  = 32'd1;
    expected[6]  = 32'd0;
    expected[7]  = 32'd1;
    expected[8]  = 32'd0;
    expected[9]  = 32'd0;
    expected[10] = 32'd0;
    expected[11] = 32'd1;
    expected[12] = 32'd1;
    expected[13] = 32'd0;
    expected[14] = 32'd0;
    expected[15] = 32'd1;
    expected[16] = 32'd0;
    expected[17] = 32'd1;
    expected[18] = 32'd1;
    expected[19] = 32'd0;
    expected[20] = 32'd0;
    expected[21] = 32'd1;
    expected[22] = 32'd1;
    expected[23] = 32'd0;
    expected[24] = 32'd1;
    expected[25] = 32'd0;
    expected[26] = 32'd0;
    expected[27] = 32'd1;
  end

  logic [31:0] cycles;
  always_ff @(posedge clk) begin
    if (areset) begin
      cycles <= 32'd0;
      done <= 1'b0;
    end else begin
      cycles <= cycles + 1;
      if (cycles == 32'd1200) begin
        bit ok;
        ok = 1'b1;
        for (int i = 0; i < 28; i = i + 1) begin
          if (Dram.sram[i] != expected[i]) begin
            ok = 1'b0;
            $display("FAIL: dmem[%0d]=%0d expected %0d", i, Dram.sram[i], expected[i]);
          end
        end
        if (ok) begin
          $display("PASS: ISA parity vector");
        end else begin
          $fatal(1);
        end
        done <= 1'b1;
      end
    end
  end

endmodule

`default_nettype wire
