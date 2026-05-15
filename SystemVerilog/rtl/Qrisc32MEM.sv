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

module Qrisc32MEM(
        input logic         clk,areset,
        qrisc32_mem_port    data_read_mem,
        qrisc32_mem_port    data_write_mem,
        input Qrisc_pack::pipe_struct_t   pipe_mem_in,
        output Qrisc_pack::pipe_struct_t  pipe_mem_out,
        output logic        pipe_stall,
        output logic        load_pending,
        output logic[4:0]   load_pending_dst,
        input logic         verbose
    );

    Qrisc_pack::pipe_struct_t pending_read;

    logic       rd_stall;
    logic       wr_stall;
    logic[31:0] addr_w;

    always_comb begin
        addr_w = pipe_mem_in.val_r1;
        pipe_stall = rd_stall | wr_stall;
        load_pending = rd_stall || pipe_mem_in.read_mem;
        if(rd_stall)
            load_pending_dst = pending_read.dst_r;
        else
            load_pending_dst = pipe_mem_in.dst_r;
    end

    always_ff@(posedge clk or posedge areset)
    if(areset) begin
        data_read_mem.address_r<='0;
        data_read_mem.rd<='0;
        data_read_mem.wr<='0;
        rd_stall<=0;
        pipe_mem_out<='0;
        pending_read<='0;
    end else begin
        if(rd_stall) begin
            data_read_mem.rd<=1'b1;
            if(!data_read_mem.wait_req) begin
// synthesis translate_off
                if(verbose)
                    $display("[MEM stage] access to dst register");
// synthesis translate_on
                rd_stall<=1'b0;
                data_read_mem.rd<=1'b0;
                pipe_mem_out<=pending_read;
                pipe_mem_out.dst_r<=pending_read.dst_r;
                pipe_mem_out.val_dst<=data_read_mem.data_r;//result of read
                pipe_mem_out.write_reg<=1'b1;
            end
        end else if(pipe_mem_in.read_mem==1) begin
            data_read_mem.address_r<=addr_w;//asserted addr
            data_read_mem.rd<=1'b1;
            pending_read<=pipe_mem_in;
            rd_stall<=1'b1;
            pipe_mem_out<='0;
        end else begin
            data_read_mem.rd<=1'b0;
            if(!wr_stall) begin
                pipe_mem_out<=pipe_mem_in;
                if(pipe_mem_in.read_mem)//if read access then
                    pipe_mem_out.write_reg<='0;//clear write bit register
            end else begin
                pipe_mem_out<='0;
            end
        end
    end

    always_ff@(posedge clk or posedge areset)
    if(areset) begin
        data_write_mem.address_r<='0;
        data_write_mem.data_w<='0;
        data_write_mem.rd<='0;
        data_write_mem.wr<='0;
        wr_stall<='0;
    end else begin
        if(wr_stall) begin
            data_write_mem.wr<=1'b1;
            if(!data_write_mem.wait_req) begin
                data_write_mem.wr<=1'b0;
                wr_stall<=1'b0;
            end
        end else if(pipe_mem_in.write_mem) begin
            data_write_mem.address_r<=addr_w;
            data_write_mem.data_w<=pipe_mem_in.val_dst;
            data_write_mem.wr<=1'b1;
            wr_stall<=1'b1;
        end else begin
            data_write_mem.wr<='0;
            wr_stall<=0;
        end
    end
endmodule

`default_nettype wire
