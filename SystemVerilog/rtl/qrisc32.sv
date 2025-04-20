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
//-------------------------------------------------------------------------------------------
// Title       : qrisc32
// Design      : qrisc32
// Author      : vinogradov@opencores.org
//4 stages risc cpu
//with 2 AXI4 interfaces:
// - Instruction Read AXI4 interface
// - Data Read/Write AXI4 interface
//--------------------------------------------------------------------------------------------
`default_nettype none

interface qrisc32_mem_port;
  logic[31:0] address_r;//address
  logic[31:0] data_r;//data is read
  logic[31:0] data_w;//data to write
  logic       rd,wr,wait_req;//read, write and wait request signals
endinterface

module qrisc32_axi_read_bridge(
    input logic         clk,
    input logic         areset,
    qrisc32_mem_port    mem,

    output logic [0:0]  arid,
    output logic [31:0] araddr,
    output logic [7:0]  arlen,
    output logic [2:0]  arsize,
    output logic [1:0]  arburst,
    output logic        arlock,
    output logic [3:0]  arcache,
    output logic [2:0]  arprot,
    output logic [3:0]  arqos,
    output logic [3:0]  arregion,
    output logic [0:0]  aruser,
    output logic        arvalid,
    input  logic        arready,

    input  logic [0:0]  rid,
    input  logic [31:0] rdata,
    input  logic [1:0]  rresp,
    input  logic        rlast,
    input  logic [0:0]  ruser,
    input  logic        rvalid,
    output logic        rready
);

    typedef enum logic [1:0] {
        R_IDLE,
        R_ADDR,
        R_RESP
    } read_state_t;

    read_state_t state;
    logic [31:0] addr_q;
    logic        ar_fire;
    logic        read_done;

    assign arid     = 1'b0;
    assign araddr   = state == R_IDLE ? mem.address_r : addr_q;
    assign arlen    = 8'd0;
    assign arsize   = 3'd2;
    assign arburst  = 2'b01;
    assign arlock   = 1'b0;
    assign arcache  = 4'd0;
    assign arprot   = 3'd0;
    assign arqos    = 4'd0;
    assign arregion = 4'd0;
    assign aruser   = 1'b0;
    assign arvalid  = (state == R_IDLE && mem.rd) || state == R_ADDR;
    assign rready   = 1'b1;

    assign ar_fire      = arvalid && arready;
    assign read_done    = (state == R_IDLE && ar_fire && rvalid) ||
                          (state == R_ADDR && ar_fire && rvalid) ||
                          (state == R_RESP && rvalid);
    assign mem.data_r   = rdata;
    assign mem.wait_req = mem.rd && !read_done;

    always_ff @(posedge clk or posedge areset) begin
        if (areset) begin
            state  <= R_IDLE;
            addr_q <= '0;
        end else begin
            unique case (state)
                R_IDLE: begin
                    if (mem.rd && !read_done) begin
                        addr_q <= mem.address_r;
                        state  <= ar_fire ? R_RESP : R_ADDR;
                    end
                end

                R_ADDR: begin
                    if (ar_fire)
                        state <= rvalid ? R_IDLE : R_RESP;
                end

                R_RESP: begin
                    if (rvalid)
                        state <= R_IDLE;
                end

                default: begin
                    state <= R_IDLE;
                end
            endcase
        end
    end
endmodule

module qrisc32_axi_write_bridge(
    input logic         clk,
    input logic         areset,
    qrisc32_mem_port    mem,

    output logic [0:0]  awid,
    output logic [31:0] awaddr,
    output logic [7:0]  awlen,
    output logic [2:0]  awsize,
    output logic [1:0]  awburst,
    output logic        awlock,
    output logic [3:0]  awcache,
    output logic [2:0]  awprot,
    output logic [3:0]  awqos,
    output logic [3:0]  awregion,
    output logic [0:0]  awuser,
    output logic        awvalid,
    input  logic        awready,

    output logic [31:0] wdata,
    output logic [3:0]  wstrb,
    output logic        wlast,
    output logic [0:0]  wuser,
    output logic        wvalid,
    input  logic        wready,

    input  logic [0:0]  bid,
    input  logic [1:0]  bresp,
    input  logic [0:0]  buser,
    input  logic        bvalid,
    output logic        bready
);

    typedef enum logic [1:0] {
        W_IDLE,
        W_ADDR_DATA,
        W_RESP
    } write_state_t;

    write_state_t state;
    logic aw_done;
    logic w_done;
    logic [31:0] addr_q;
    logic [31:0] data_q;
    logic aw_fire;
    logic w_fire;
    logic write_done;

    assign awid     = 1'b0;
    assign awaddr   = state == W_IDLE ? mem.address_r : addr_q;
    assign awlen    = 8'd0;
    assign awsize   = 3'd2;
    assign awburst  = 2'b01;
    assign awlock   = 1'b0;
    assign awcache  = 4'd0;
    assign awprot   = 3'd0;
    assign awqos    = 4'd0;
    assign awregion = 4'd0;
    assign awuser   = 1'b0;
    assign awvalid  = (state == W_IDLE && mem.wr) || (state == W_ADDR_DATA && !aw_done);

    assign wdata    = state == W_IDLE ? mem.data_w : data_q;
    assign wstrb    = 4'b1111;
    assign wlast    = 1'b1;
    assign wuser    = 1'b0;
    assign wvalid   = (state == W_IDLE && mem.wr) || (state == W_ADDR_DATA && !w_done);

    assign bready   = 1'b1;

    assign aw_fire = awvalid && awready;
    assign w_fire = wvalid && wready;
    assign write_done =
        (state == W_IDLE && aw_fire && w_fire && bvalid) ||
        (state == W_ADDR_DATA && (aw_done || aw_fire) && (w_done || w_fire) && bvalid) ||
        (state == W_RESP && bvalid);

    assign mem.wait_req = mem.wr && !write_done;

    always_ff @(posedge clk or posedge areset) begin
        if (areset) begin
            state   <= W_IDLE;
            aw_done <= 1'b0;
            w_done  <= 1'b0;
            addr_q  <= '0;
            data_q  <= '0;
        end else begin
            unique case (state)
                W_IDLE: begin
                    aw_done <= 1'b0;
                    w_done  <= 1'b0;
                    if (mem.wr) begin
                        addr_q <= mem.address_r;
                        data_q <= mem.data_w;
                        if (!write_done) begin
                            aw_done <= aw_fire;
                            w_done <= w_fire;
                            state <= (aw_fire && w_fire) ? W_RESP : W_ADDR_DATA;
                        end
                    end
                end

                W_ADDR_DATA: begin
                    if (aw_fire)
                        aw_done <= 1'b1;
                    if (w_fire)
                        w_done <= 1'b1;
                    if ((aw_done || aw_fire) && (w_done || w_fire))
                        state <= bvalid ? W_IDLE : W_RESP;
                end

                W_RESP: begin
                    if (bvalid)
                        state <= W_IDLE;
                end

                default: begin
                    state <= W_IDLE;
                end
            endcase
        end
    end
endmodule

module qrisc32(
    input logic         clk,areset,

    //AXI4 instruction read channel
    output logic [0:0]  axi_instruction_arid,
    output logic [31:0] axi_instruction_araddr,
    output logic [7:0]  axi_instruction_arlen,
    output logic [2:0]  axi_instruction_arsize,
    output logic [1:0]  axi_instruction_arburst,
    output logic        axi_instruction_arlock,
    output logic [3:0]  axi_instruction_arcache,
    output logic [2:0]  axi_instruction_arprot,
    output logic [3:0]  axi_instruction_arqos,
    output logic [3:0]  axi_instruction_arregion,
    output logic [0:0]  axi_instruction_aruser,
    output logic        axi_instruction_arvalid,
    input  logic        axi_instruction_arready,

    input  logic [0:0]  axi_instruction_rid,
    input  logic [31:0] axi_instruction_rdata,
    input  logic [1:0]  axi_instruction_rresp,
    input  logic        axi_instruction_rlast,
    input  logic [0:0]  axi_instruction_ruser,
    input  logic        axi_instruction_rvalid,
    output logic        axi_instruction_rready,

    //AXI4 data read channel
    output logic [0:0]  axi_data_arid,
    output logic [31:0] axi_data_araddr,
    output logic [7:0]  axi_data_arlen,
    output logic [2:0]  axi_data_arsize,
    output logic [1:0]  axi_data_arburst,
    output logic        axi_data_arlock,
    output logic [3:0]  axi_data_arcache,
    output logic [2:0]  axi_data_arprot,
    output logic [3:0]  axi_data_arqos,
    output logic [3:0]  axi_data_arregion,
    output logic [0:0]  axi_data_aruser,
    output logic        axi_data_arvalid,
    input  logic        axi_data_arready,

    input  logic [0:0]  axi_data_rid,
    input  logic [31:0] axi_data_rdata,
    input  logic [1:0]  axi_data_rresp,
    input  logic        axi_data_rlast,
    input  logic [0:0]  axi_data_ruser,
    input  logic        axi_data_rvalid,
    output logic        axi_data_rready,

    //AXI4 data write channel
    output logic [0:0]  axi_data_awid,
    output logic [31:0] axi_data_awaddr,
    output logic [7:0]  axi_data_awlen,
    output logic [2:0]  axi_data_awsize,
    output logic [1:0]  axi_data_awburst,
    output logic        axi_data_awlock,
    output logic [3:0]  axi_data_awcache,
    output logic [2:0]  axi_data_awprot,
    output logic [3:0]  axi_data_awqos,
    output logic [3:0]  axi_data_awregion,
    output logic [0:0]  axi_data_awuser,
    output logic        axi_data_awvalid,
    input  logic        axi_data_awready,

    output logic [31:0] axi_data_wdata,
    output logic [3:0]  axi_data_wstrb,
    output logic        axi_data_wlast,
    output logic [0:0]  axi_data_wuser,
    output logic        axi_data_wvalid,
    input  logic        axi_data_wready,

    input  logic [0:0]  axi_data_bid,
    input  logic [1:0]  axi_data_bresp,
    input  logic [0:0]  axi_data_buser,
    input  logic        axi_data_bvalid,
    output logic        axi_data_bready,

    input  logic        verbose//for simlation

    );

    qrisc32_mem_port instruction_mem(),data_read_mem(),data_write_mem();

    qrisc32_axi_read_bridge instruction_axi_bridge(
        .clk(clk),
        .areset(areset),
        .mem(instruction_mem),
        .arid(axi_instruction_arid),
        .araddr(axi_instruction_araddr),
        .arlen(axi_instruction_arlen),
        .arsize(axi_instruction_arsize),
        .arburst(axi_instruction_arburst),
        .arlock(axi_instruction_arlock),
        .arcache(axi_instruction_arcache),
        .arprot(axi_instruction_arprot),
        .arqos(axi_instruction_arqos),
        .arregion(axi_instruction_arregion),
        .aruser(axi_instruction_aruser),
        .arvalid(axi_instruction_arvalid),
        .arready(axi_instruction_arready),
        .rid(axi_instruction_rid),
        .rdata(axi_instruction_rdata),
        .rresp(axi_instruction_rresp),
        .rlast(axi_instruction_rlast),
        .ruser(axi_instruction_ruser),
        .rvalid(axi_instruction_rvalid),
        .rready(axi_instruction_rready)
    );

    qrisc32_axi_read_bridge data_read_axi_bridge(
        .clk(clk),
        .areset(areset),
        .mem(data_read_mem),
        .arid(axi_data_arid),
        .araddr(axi_data_araddr),
        .arlen(axi_data_arlen),
        .arsize(axi_data_arsize),
        .arburst(axi_data_arburst),
        .arlock(axi_data_arlock),
        .arcache(axi_data_arcache),
        .arprot(axi_data_arprot),
        .arqos(axi_data_arqos),
        .arregion(axi_data_arregion),
        .aruser(axi_data_aruser),
        .arvalid(axi_data_arvalid),
        .arready(axi_data_arready),
        .rid(axi_data_rid),
        .rdata(axi_data_rdata),
        .rresp(axi_data_rresp),
        .rlast(axi_data_rlast),
        .ruser(axi_data_ruser),
        .rvalid(axi_data_rvalid),
        .rready(axi_data_rready)
    );

    qrisc32_axi_write_bridge data_write_axi_bridge(
        .clk(clk),
        .areset(areset),
        .mem(data_write_mem),
        .awid(axi_data_awid),
        .awaddr(axi_data_awaddr),
        .awlen(axi_data_awlen),
        .awsize(axi_data_awsize),
        .awburst(axi_data_awburst),
        .awlock(axi_data_awlock),
        .awcache(axi_data_awcache),
        .awprot(axi_data_awprot),
        .awqos(axi_data_awqos),
        .awregion(axi_data_awregion),
        .awuser(axi_data_awuser),
        .awvalid(axi_data_awvalid),
        .awready(axi_data_awready),
        .wdata(axi_data_wdata),
        .wstrb(axi_data_wstrb),
        .wlast(axi_data_wlast),
        .wuser(axi_data_wuser),
        .wvalid(axi_data_wvalid),
        .wready(axi_data_wready),
        .bid(axi_data_bid),
        .bresp(axi_data_bresp),
        .buser(axi_data_buser),
        .bvalid(axi_data_bvalid),
        .bready(axi_data_bready)
    );

    risc_pack::pipe_struct_t
                pipe_id_out,//I decode
                pipe_ex_out,//Ex
                pipe_mem_out;//MEM access

    logic[31:0]  instruction,pc;
    logic        new_address_valid_ex;
    logic[31:0]  new_address_ex;

    logic        new_address_valid_mem;
    logic[31:0]  new_address_mem;

    logic        pipe_stall;

    qrisc32_IF  qrisc32_IF(
        .clk(clk),
        .areset(areset),
        .pipe_stall(pipe_stall),
        .instruction_mem(instruction_mem),
        .new_address_valid(new_address_valid_ex),
        .new_address(new_address_ex),

        .instruction(instruction),
        .pc(pc)
    );

    qrisc32_ID  qrisc32_ID(
        .clk(clk),
        .areset(areset),
        .pipe_stall(pipe_stall),
        .instruction(instruction),
        .pc(pc),
        .pipe_wb_mem(pipe_mem_out),//for memory read
        .pipe_wb_ex(pipe_ex_out),//for R2 register and ALU operations only
        .pipe_id_out(pipe_id_out),
        .verbose(verbose)
    );

    qrisc32_EX  qrisc32_EX(
        .clk(clk),
        .areset(areset),
        .pipe_stall(pipe_stall),
        .pipe_ex_in(pipe_id_out),
        .pipe_ex_out(pipe_ex_out),
        .new_address_valid(new_address_valid_ex),
        .new_address(new_address_ex)
    );

    qrisc32_MEM qrisc32_MEM(
        .clk(clk),
        .areset(areset),
        .pipe_mem_in(pipe_ex_out),
        .data_read_mem(data_read_mem),
        .data_write_mem(data_write_mem),
        .pipe_mem_out(pipe_mem_out),
        .pipe_stall(pipe_stall),
        .verbose(verbose)
    );

endmodule

`default_nettype wire
