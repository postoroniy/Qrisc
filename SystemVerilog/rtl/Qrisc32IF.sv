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
//
//////////////////////////////////////////////////////////////////////////////////////////////
`default_nettype none

module Qrisc32IF(
    input logic       clk,areset,
    qrisc32_mem_port  instruction_mem,

    input logic       pipe_stall,//feed back from MEM stage

    input logic       new_address_valid,//feed back from  EX stage
    input logic[31:0] new_address,//feed back from  EX stage

    output logic[31:0]  instruction,
    output logic[31:0]  pc
    );

    logic[2:0]  offset_w;//
    logic[31:0] base_w;//
    logic[31:0] jump_address_w;
    logic       if_stall;
    logic[31:0] stalled_adr0;
    logic[31:0] offset_ext_w;

    always_comb begin
        if_stall = instruction_mem.wait_req | pipe_stall;
        pc       = instruction_mem.address_r;
        base_w   = instruction_mem.address_r;
        offset_w=4;
        if(if_stall)
            offset_w=0;
        else if(new_address_valid) begin
            base_w=new_address;
            offset_w=0;
        end
        offset_ext_w   = {29'd0, offset_w};
        jump_address_w = base_w + offset_ext_w;
    end

    always_ff@(posedge clk or posedge areset)
    if(areset) begin
        instruction_mem.address_r<='0;//reset address!
        instruction_mem.rd<=1;//forever =1
        instruction<='0;//=ldr R0,R0 =nop
        stalled_adr0<='0;
    end else begin
        if(~if_stall) begin
            instruction                 <= instruction_mem.data_r;
            instruction_mem.address_r  <= jump_address_w;
            stalled_adr0                <= jump_address_w;
        end else begin
            instruction_mem.address_r  <= stalled_adr0;
            //instruction<='0;
            //instruction<=instruction_mem.data_r;
//synthesys translate_off
            $display("[IF stage] Stalled->%s",(pipe_stall)?"Mem stage is stalled":"Fetching is stalled");
//synthesys translate_on
        end
    end
endmodule
`default_nettype wire
