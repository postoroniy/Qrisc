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

module qrisc32_flags_tb(
  input   logic   clk,
  input   logic   areset,
  output  logic   Istop_active,
  output  logic   Dstop_active,
  input   bit     Istop_enable,
  input   bit     Dstop_enable,
  input   bit     verbose,
  output  bit     done = 0
);

  import risc_pack::*;

  localparam int IMEM_SIZE = 256;
  localparam int IMEM_ADR_LIMIT = 128;
  localparam int DMEM_SIZE = 256;
  localparam int DMEM_ADR_LIMIT = 256;

`include "axi_tb_connect.svh"

  function automatic logic [31:0] mkLdrh(input logic [15:0] imm, input logic [4:0] dst);
    logic [31:0] inst;
    inst = '0;
    inst[31:28] = LDR;
    inst[27:26] = 2'b01;
    inst[25]    = 1'b0;
    inst[24:22] = 3'b000;
    inst[21]    = 1'b0;
    inst[20:5]  = imm;
    inst[4:0]   = dst;
    return inst;
  endfunction

  function automatic logic [31:0] mkLdrl(input logic [15:0] imm, input logic [4:0] dst);
    logic [31:0] inst;
    inst = '0;
    inst[31:28] = LDR;
    inst[27:26] = 2'b10;
    inst[25]    = 1'b0;
    inst[24:22] = 3'b000;
    inst[21]    = 1'b0;
    inst[20:5]  = imm;
    inst[4:0]   = dst;
    return inst;
  endfunction

  function automatic logic [31:0] mkAlu(input logic [2:0] subop, input logic [2:0] inc, input logic [4:0] src2, input logic [4:0] src1, input logic [4:0] dst);
    return {ALU, subop, inc, 7'h0, src2, src1, dst};
  endfunction

  function automatic logic [31:0] mkStrp(input logic signed [14:0] off15, input logic [4:0] base, input logic [4:0] dst);
    logic [31:0] inst;
    inst = '0;
    inst[31:28] = STR;
    inst[27:26] = 2'b11;
    inst[25]    = 1'b0;
    inst[24:10] = off15;
    inst[9:5]   = base;
    inst[4:0]   = dst;
    return inst;
  endfunction

  function automatic logic [31:0] mkLdrf(input logic [1:0] cond, input logic [2:0] inc, input logic [4:0] src2, input logic [4:0] src1, input logic [4:0] dst);
    logic [31:0] inst;
    inst = '0;
    inst[31:28] = LDRF;
    inst[27:26] = cond;
    inst[25]    = 1'b0;
    inst[24:22] = inc;
    inst[21:15] = 7'h0;
    inst[14:10] = src2;
    inst[9:5]   = src1;
    inst[4:0]   = dst;
    return inst;
  endfunction

  initial begin : init_mem
    integer i;
    for (i = 0; i < 256; i = i + 1) begin
      Iram.sram[i] = 32'h00000000;
      Dram.sram[i] = 32'h00000000;
    end

    $readmemh("../tests/flags.hex", Iram.sram);
  end

  logic [31:0] cycles;
  always_ff @(posedge clk) begin
    if (areset) begin
      cycles <= 32'd0;
      done <= 1'b0;
    end else begin
      cycles <= cycles + 1;
      if (cycles == 32'd250) begin
        if (Dram.sram[0] == 32'd5 && Dram.sram[1] == 32'd7) begin
          $display("PASS: dmem[0]=%0d dmem[1]=%0d", Dram.sram[0], Dram.sram[1]);
        end else begin
          $display("FAIL: dmem[0]=%0d dmem[1]=%0d (expected 5,7)", Dram.sram[0], Dram.sram[1]);
          $fatal(1);
        end
        done <= 1'b1;
      end
    end
  end

endmodule

`default_nettype wire
