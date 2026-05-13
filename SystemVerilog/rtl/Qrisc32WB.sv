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

module Qrisc32WB(
    input  logic                    clk,
    input  logic                    areset,
    input  Qrisc_pack::pipe_struct_t pipe_wb_ex_in,
    input  Qrisc_pack::pipe_struct_t pipe_wb_mem_in,
    output Qrisc_pack::pipe_struct_t pipe_wb_ex_out,
    output Qrisc_pack::pipe_struct_t pipe_wb_mem_out
);

    always_ff @(posedge clk or posedge areset) begin
        if(areset) begin
            pipe_wb_ex_out  <= '0;
            pipe_wb_mem_out <= '0;
        end else begin
            pipe_wb_ex_out  <= pipe_wb_ex_in;
            pipe_wb_mem_out <= pipe_wb_mem_in;
        end
    end

endmodule

`default_nettype wire
