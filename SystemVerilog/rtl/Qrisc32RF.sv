//////////////////////////////////////////////////////////////////////////////////////////////
//    Project Qrisc32 is risc cpu implementation, purpose is studying
//    Digital System Design course at Kyoung Hee University during my PhD earning
//    Copyright (C) 2010-2026  Viacheslav Vinogradov
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
//////////////////////////////////////////////////////////////////////////////////////////////
`default_nettype none

module Qrisc32RF(
    input  logic                  clk,
    input  logic                  areset,
    input  logic[4:0]             src_r1_addr,
    input  logic[4:0]             src_r2_addr,
    input  logic[4:0]             dst_addr,
    output logic[31:0]            src_r1_data,
    output logic[31:0]            src_r2_data,
    output logic[31:0]            dst_data,
    input  Qrisc_pack::pipe_struct_t pipe_wb_ex,
    input  Qrisc_pack::pipe_struct_t pipe_wb_mem
);

    logic[31:0] rf[32];

    assign src_r1_data = rf[src_r1_addr];
    assign src_r2_data = rf[src_r2_addr];
    assign dst_data    = rf[dst_addr];

    always_ff @(posedge clk or posedge areset) begin
        if(areset) begin
            for(int i=0;i<32;i++)
                rf[i] <= '0;
        end else begin
            if(pipe_wb_ex.write_reg)
                rf[pipe_wb_ex.dst_r] <= pipe_wb_ex.val_dst;

            if(pipe_wb_ex.incr_r2_enable)
                rf[pipe_wb_ex.src_r2] <= pipe_wb_ex.val_r2;

            if(pipe_wb_mem.write_reg)
                rf[pipe_wb_mem.dst_r] <= pipe_wb_mem.val_dst;
        end
    end

endmodule

`default_nettype wire
