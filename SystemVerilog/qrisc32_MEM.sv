///////////////////////////////////////////////////////////////////////////////
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
///////////////////////////////////////////////////////////////////////////////
`default_nettype none
`timescale 1ns / 1ns

module qrisc32_MEM(
        input logic         clk,areset,
        avalon_port         avm_data_read,//avalon master port only for  reading data
        avalon_port         avm_data_write,//avalon master port only for  writing data
        input risc_pack::pipe_struct_t   pipe_mem_in,
        output risc_pack::pipe_struct_t  pipe_mem_out,
        output logic        pipe_stall,
        input logic         verbose
    );

    risc_pack::pipe_struct_t pipe_mem_in0,pipe_mem_in1;

    logic       rd_stall;
    logic       wr_stall;
    logic[31:0] addr_w;

    always_comb begin
        addr_w = pipe_mem_in.val_r1;
        pipe_stall = rd_stall | wr_stall;
    end

    always_ff@(posedge clk or posedge areset)
    if(areset) begin
        avm_data_read.address_r<='0;
        avm_data_read.rd<='0;
        avm_data_read.wr<='0;
        rd_stall<=0;
        pipe_mem_out<='0;
        pipe_mem_in0<='0;
        pipe_mem_in1<='0;
    end else begin
        if(pipe_mem_in.read_mem==1)
            avm_data_read.address_r<=addr_w;//asserted addr

        avm_data_read.rd<=pipe_mem_in.read_mem;//||pipe_mem_in0.read_mem||pipe_mem_in1.read_mem;
        pipe_mem_in0<=pipe_mem_in;//addr asserted
        pipe_mem_in1<=pipe_mem_in0;//just wait cycle

        if(avm_data_read.wait_req==0 && pipe_mem_in1.read_mem==1)begin
        // it has been read ok(sram access is 2 cycles)
            //if(~pipe_mem_in.write_reg)
            begin
                if(verbose)
                    $display("[MEM stage] access to dst register");
                rd_stall<=0;
                pipe_mem_out<=pipe_mem_in;
                pipe_mem_out.dst_r<=pipe_mem_in1.dst_r;
                pipe_mem_out.val_dst<=avm_data_read.data_r;//result of read
                pipe_mem_out.write_reg<=1;
            end
            //else
            //if(~pipe_mem_in.incr_r2_enable)
            //begin
            //    if(verbose)
            //        $display("[MEM stage] access to src2 register");
            //    rd_stall<=0;
            //    pipe_mem_out<=pipe_mem_in;
            //
            //    pipe_mem_out.src_r2<=pipe_mem_in1.dst_r;
            //    pipe_mem_out.incr_r2_enable<=1;
            //    pipe_mem_out.val_r2<=avm_data_read.data_r;//result of read
            //end
            //else
            //begin
            //    if(verbose)
            //        $display("[MEM stage] access to dst register and STALL pipeline!");
            //    rd_stall<=1;
            //    pipe_mem_out<=pipe_mem_in1;
            //    pipe_mem_out.val_dst<=avm_data_read.data_r;//result of read
            //end
        end else begin
            rd_stall<=0;
            //if previous op was stalled
            if(rd_stall)begin
                pipe_mem_out<=pipe_mem_in0;
            end else begin
                pipe_mem_out<=pipe_mem_in;
                if(pipe_mem_in.read_mem)//if read access then
                    pipe_mem_out.write_reg<='0;//clear write bit register
            end
        end
    end

    always_ff@(posedge clk or posedge areset)
    if(areset) begin
        avm_data_write.address_r<='0;
        avm_data_write.data_w<='0;
        avm_data_write.rd<='0;
        avm_data_write.wr<='0;
        wr_stall<='0;
    end else begin
        if(pipe_mem_in.write_mem) begin
            avm_data_write.address_r<=addr_w;
            avm_data_write.data_w<=pipe_mem_in.val_dst;
            avm_data_write.wr<=1;
            wr_stall<=0;
        end else begin
            avm_data_write.wr<='0;
            wr_stall<=0;
        end
    end
endmodule

`default_nettype wire
