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

module qrisc32_EX(
    input  logic                  clk,areset,pipe_stall,
    input  risc_pack::pipe_struct_t pipe_ex_in,
    output risc_pack::pipe_struct_t pipe_ex_out,

    output logic          new_address_valid,//to mem stage
    output logic[31:0]    new_address//to mem stage
    );

    logic   flagZ_w, flagC_w;
    logic   flagZ, flagC;
    logic signed[31:0]   r2;
    logic signed[3:0]    inc_r2;
    logic signed[31:0]   r2_add;
    logic[32:0]  summ_result;

    risc_pack::pipe_struct_t pipe_ex_out_w;

    always_comb begin
        r2      = pipe_ex_in.val_r2;
        inc_r2  = pipe_ex_in.incr_r2;
        r2_add  = r2 + inc_r2;
        summ_result = pipe_ex_in.val_r1 + pipe_ex_in.val_r2;
        //
        pipe_ex_out_w=pipe_ex_in;
        flagZ_w=0;
        flagC_w=0;

        if(pipe_ex_in.ldrf_op) begin
            if(   (pipe_ex_in.jmpz  && flagZ)
                ||(pipe_ex_in.jmpnz && ~flagZ)
                ||(pipe_ex_in.jmpc  && flagC)
                ||(pipe_ex_in.jmpnc && ~flagC)
                )
                pipe_ex_out_w.val_dst=pipe_ex_in.val_r1;
            else
                pipe_ex_out_w.val_dst=pipe_ex_in.val_r2;
        end else if(pipe_ex_in.and_op) begin
            pipe_ex_out_w.val_dst=pipe_ex_in.val_r1 & pipe_ex_in.val_r2;
            flagC_w=0;
            flagZ_w=(pipe_ex_out_w.val_dst==0)?1:0;
        end else if(pipe_ex_in.or_op) begin
            pipe_ex_out_w.val_dst=pipe_ex_in.val_r1 | pipe_ex_in.val_r2;
            flagC_w=0;
            flagZ_w=(pipe_ex_out_w.val_dst==0)?1:0;
        end else if(pipe_ex_in.xor_op) begin
            pipe_ex_out_w.val_dst=pipe_ex_in.val_r1 ^ pipe_ex_in.val_r2;
            flagC_w=0;
            flagZ_w=(pipe_ex_out_w.val_dst==0)?1:0;
        end else if(   pipe_ex_in.add_op//simple addition operation
                || pipe_ex_in.jmpunc //jump unconditional calculate address = R1+R2(offset)
                //jump conditional calculate address = R1+R2(offset) and check flags
                || pipe_ex_in.jmpz || pipe_ex_in.jmpnz || pipe_ex_in.jmpc || pipe_ex_in.jmpnc)
        begin
            {flagC_w,pipe_ex_out_w.val_dst}=summ_result;
            flagZ_w=(pipe_ex_out_w.val_dst==0)?1:0;
        end else if(pipe_ex_in.mul_op) begin
            {flagC_w,pipe_ex_out_w.val_dst}=pipe_ex_in.val_r1 * pipe_ex_in.val_r2;
            flagZ_w=(pipe_ex_out_w.val_dst==0)?1:0;
        end else if(pipe_ex_in.shl_op) begin
            {flagC_w,pipe_ex_out_w.val_dst}=pipe_ex_in.val_r1 << pipe_ex_in.val_r2;
            flagZ_w=(pipe_ex_out_w.val_dst==0)?1:0;
        end else if(pipe_ex_in.shr_op) begin
            {pipe_ex_out_w.val_dst,flagC_w}={pipe_ex_in.val_r1,1'b0 }>> pipe_ex_in.val_r2;
            flagZ_w=(pipe_ex_out_w.val_dst==0)?1:0;
        end else if(pipe_ex_in.cmp_op) begin
            flagZ_w=(pipe_ex_out_w.val_r1==pipe_ex_out_w.val_r2)?1:0;
            flagC_w=(pipe_ex_out_w.val_r1>=pipe_ex_out_w.val_r2)?0:1;
        end
        pipe_ex_out_w.val_r2 = (pipe_ex_out_w.incr_r2_enable)?r2_add:pipe_ex_out_w.val_r2;
    end

    always_ff@(posedge clk or posedge areset)
    if(areset)begin
        {flagZ,flagC,new_address_valid,new_address,pipe_ex_out}<='0;
    end else begin
        flagZ<=
        (pipe_ex_in.and_op | pipe_ex_in.or_op | pipe_ex_in.xor_op | pipe_ex_in.add_op
        | pipe_ex_in.mul_op | pipe_ex_in.shl_op | pipe_ex_in.shr_op | pipe_ex_in.cmp_op)?flagZ_w:flagZ;

        flagC<=
        (pipe_ex_in.and_op | pipe_ex_in.or_op | pipe_ex_in.xor_op | pipe_ex_in.add_op
        | pipe_ex_in.mul_op | pipe_ex_in.shl_op | pipe_ex_in.shr_op | pipe_ex_in.cmp_op)?flagC_w:flagC;

        if(pipe_ex_in.jmpunc || (pipe_ex_in.jmpz & flagZ) ||(pipe_ex_in.jmpnz & !flagZ)||
            (pipe_ex_in.jmpc & flagC) ||(pipe_ex_in.jmpnc & !flagC) )
        begin
            new_address_valid<=~pipe_ex_in.ldrf_op;
            new_address<=pipe_ex_out_w.val_dst;
        end
        else
            new_address_valid<=0;

        if(~pipe_stall)
        begin
            pipe_ex_out<=pipe_ex_out_w;
            if(pipe_ex_in.read_mem| pipe_ex_in.write_mem)
                pipe_ex_out.val_r1<=summ_result;//address for accessing
        end
    end
endmodule

`default_nettype wire
