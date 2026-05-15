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

    input logic       mem_stall,//feed back from MEM stage
    input logic       hazard_stall,

    input logic       new_address_valid,//feed back from  EX stage
    input logic[31:0] new_address,//feed back from  EX stage

    output logic[31:0]  instruction,
    output logic[31:0]  pc,
    input logic         verbose
    );

    typedef enum logic {
        IF_FETCH,
        IF_HOLD
    } if_state_t;

    if_state_t state;
    logic[31:0] fetch_pc;
    logic[31:0] hold_pc;
    logic       id_ready;

    always_comb begin
        id_ready = !mem_stall && !hazard_stall;
        pc       = hold_pc + 32'd8;
    end

    always_ff@(posedge clk or posedge areset)
    if(areset) begin
        state <= IF_FETCH;
        instruction_mem.address_r <= '0;
        instruction_mem.rd <= 1'b0;
        instruction <= '0;
        fetch_pc <= '0;
        hold_pc <= '0;
    end else begin
        if(new_address_valid) begin
            state <= IF_FETCH;
            instruction <= '0;
            fetch_pc <= new_address;
            hold_pc <= new_address;
            instruction_mem.address_r <= new_address;
            instruction_mem.rd <= 1'b1;
        end else begin
            unique case(state)
                IF_FETCH: begin
                    instruction_mem.address_r <= fetch_pc;
                    instruction_mem.rd <= 1'b1;
                    instruction <= '0;
                    hold_pc <= fetch_pc;
                    if(!instruction_mem.wait_req && instruction_mem.rd) begin
                        instruction <= instruction_mem.data_r;
                        fetch_pc <= fetch_pc + 32'd4;
                        state <= IF_HOLD;
                        instruction_mem.rd <= 1'b0;
                    end
                end

                IF_HOLD: begin
                    instruction_mem.rd <= 1'b0;
                    if(id_ready) begin
                        instruction <= '0;
                        state <= IF_FETCH;
                    end
                end
            endcase
// synthesis translate_off
            if(verbose && (mem_stall || hazard_stall || (state == IF_FETCH && instruction_mem.wait_req)))
                $display("[IF stage] Stalled->%s",(mem_stall)?"Mem stage is stalled":((hazard_stall)?"Hazard is stalled":"Fetching is stalled"));
// synthesis translate_on
        end
    end
endmodule
`default_nettype wire
