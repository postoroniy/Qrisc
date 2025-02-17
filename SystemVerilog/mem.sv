//////////////////////////////////////////////////////////////////////////////////////////////
//    Project Qrisc32 is risc cpu implementation, purpose is studying
//    Digital System Design course at Kyoung Hee University during my PhD earning
//    Copyright (C) 2010-2012  Vinogradov Viacheslav
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

module mem
#(parameter size=256,adr_limit=64)(
    input logic          clk,
    input logic[31:0]    add_r,add_w,data_w,
    input logic          rd,wr,
    output bit[31:0]     data_r,
    output logic         req

    ,input  logic       stop_enable//=1 will stop on each problem(non align access and out of adr access)
    ,output bit         stop_active//=1 will indicate about stop signal
    ,input  logic       verbose
    );

    bit[31:0] sram[size];

    assign    req        =    1'b0;
    wire[$bits(size)-1:0]add_r_word =add_r[31:2];
    wire[$bits(size)-1:0]add_w_word =add_w[31:2];

    always_ff@(posedge clk) begin
        stop_active=0;
        if(wr) begin
            sram[add_w_word]<=data_w;
            if(verbose)
                $display(".................................[%m] write at address %08x with data %08x",add_w,data_w);
        end
        data_r    <=    sram[add_r_word];

        if(rd) begin
            if(verbose)
                $display(".................................[%m] read at address %08x with data %08x",add_r,sram[add_r_word]);
        end

        if(wr && add_w>adr_limit*4) begin
            if(verbose)
                $display(".................................[%m] write out of limit address %08x",add_w);
            stop_active=1;
            // if(stop_enable)$stop;
        end

        if(rd && add_r>adr_limit*4)begin
            if(verbose)
                $display(".................................[%m] read out of limit address %08x",add_r);
            // if(stop_enable)$stop;
            stop_active=1;
        end

        if(add_r[1:0]!=0 && rd) begin
            if(verbose)
                $display(".................................[%m] read by non align address  %08x",add_r);
            // if(stop_enable)$stop;
            stop_active=1;
        end

        if(add_w[1:0]!=0 && wr) begin
            if(verbose)
                $display(".................................[%m] write by non align address %08x",add_w);
            // if(stop_enable)$stop;
            stop_active=1;
        end
    end
endmodule

`default_nettype wire
