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
`timescale 1ns / 1ns

module qrisc32_IF(
    input logic       clk,areset,
    avalon_port       avm_instructions,//avalon master port only for  reading instructions

    input logic       pipe_stall,//feed back from MEM stage

    input logic       new_address_valid,//feed back from  EX stage
    input logic[31:0] new_address,//feed back from  EX stage

    output bit[31:0]  instruction,
    output bit[31:0]  pc
    );


    bit[2:0]    offset_w;//
    bit[31:0]   base_w;//
    wire[31:0]  jump_address_w = base_w+offset_w;

    wire        if_stall       = avm_instructions.wait_req | pipe_stall;

    bit[31:0]   stalled_adr0;
    bit[31:0]   stalled_adr1;

    always_comb begin
        pc=avm_instructions.address_r;//

        base_w=avm_instructions.address_r;
        offset_w=4;
        if(if_stall)
            offset_w=0;
        else if(new_address_valid) begin
            base_w=new_address;
            offset_w=0;
        end
    end

    always_ff@(posedge clk or posedge areset)
    if(areset) begin
        avm_instructions.address_r<='0;//reset address!
        avm_instructions.rd<=1;//forever =1
        instruction<='0;//=ldr R0,R0 =nop
    end else begin
        if(~if_stall) begin
            instruction<=avm_instructions.data_r;
            avm_instructions.address_r<=jump_address_w;
            stalled_adr0<=jump_address_w;
            stalled_adr1<=stalled_adr0;
        end else begin
            avm_instructions.address_r<=stalled_adr0;
            //instruction<='0;
            //instruction<=avm_instructions.data_r;
//synthesys translate_off
            $display("[IF stage] Stalled->%s",(pipe_stall)?"Mem stage is stalled":"Fetching is stalled");
//synthesys translate_on
        end
    end
endmodule
`default_nettype wire
