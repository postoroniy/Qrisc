//////////////////////////////////////////////////////////////////////////////////////////////
// Checked single-beat AXI4 memory models for qrisc32 protocol testbenches.
//////////////////////////////////////////////////////////////////////////////////////////////
`default_nettype none
`timescale 1ns / 1ns

module axi_checked_read_mem
#(
    parameter int size = 256,
    parameter int adr_limit = 64,
    parameter int ready_phase = 0
)(
    input  logic        clk,
    input  logic        areset,

    input  logic [0:0]  arid,
    input  logic [31:0] araddr,
    input  logic [7:0]  arlen,
    input  logic [2:0]  arsize,
    input  logic [1:0]  arburst,
    input  logic        arlock,
    input  logic [3:0]  arcache,
    input  logic [2:0]  arprot,
    input  logic [3:0]  arqos,
    input  logic [3:0]  arregion,
    input  logic [0:0]  aruser,
    input  logic        arvalid,
    output logic        arready,

    output logic [0:0]  rid,
    output logic [31:0] rdata,
    output logic [1:0]  rresp,
    output logic        rlast,
    output logic [0:0]  ruser,
    output logic        rvalid,
    input  logic        rready,

    input  logic        stop_enable,
    output bit          stop_active,
    input  logic        verbose
);

    localparam int ADDR_W = $clog2(size);

    bit [31:0] sram[size];

    logic [31:0] cycle = 32'd0;
    logic [31:0] req_count = 32'd0;
    logic        rd_pending = 1'b0;
    logic [3:0]  rd_delay = 4'd0;
    logic [0:0]  rid_q = 1'b0;
    logic [31:0] rdata_q = 32'd0;

    logic        hold_ar = 1'b0;
    logic [62:0] held_ar;
    wire  [62:0] ar_payload = {arid, araddr, arlen, arsize, arburst, arlock,
                               arcache, arprot, arqos, arregion, aruser};
    wire  [ADDR_W-1:0] araddr_word = araddr[ADDR_W+1:2];

    assign arready = !rd_pending && ((cycle + ready_phase) % 4 != 1);
    assign rid     = rid_q;
    assign rdata   = rdata_q;
    assign rresp   = 2'b00;
    assign rlast   = 1'b1;
    assign ruser   = 1'b0;
    assign rvalid  = rd_pending && rd_delay == 0;

    task automatic fail(input string msg);
        begin
            $display("FAIL: %m: %s", msg);
            stop_active = 1'b1;
            if (stop_enable)
                $fatal(1);
            else
                $fatal(1);
        end
    endtask

    always_ff @(posedge clk) begin
        cycle <= cycle + 1;
        stop_active = 1'b0;

        if (areset) begin
            rd_pending <= 1'b0;
            rd_delay <= 4'd0;
            hold_ar <= 1'b0;
        end else begin
        if (arvalid && !arready) begin
            if (hold_ar && held_ar != ar_payload)
                fail("AR payload changed while stalled");
            hold_ar <= 1'b1;
            held_ar <= ar_payload;
        end else begin
            hold_ar <= 1'b0;
        end

        if (rd_pending && arvalid)
            fail("new AR issued before read response");

        if (arvalid && arready) begin
            if (arlen != 8'd0)
                fail("ARLEN is not single beat");
            if (arsize != 3'd2)
                fail("ARSIZE is not 32-bit");
            if (arburst != 2'b01)
                fail("ARBURST is not INCR");
            if (araddr[1:0] != 2'b00)
                fail("unaligned read address");
            if (araddr > adr_limit * 4)
                fail("read address out of limit");

            rd_pending <= 1'b1;
            rd_delay <= req_count[0] ? 4'd2 : 4'd4;
            req_count <= req_count + 1;
            rid_q <= arid;
            rdata_q <= sram[araddr_word];
            if (verbose)
                $display("[%m] AXI read accepted address %08x data %08x", araddr, sram[araddr_word]);
        end else if (rd_pending && rd_delay != 0) begin
            rd_delay <= rd_delay - 1;
        end

        if (rvalid && rready)
            rd_pending <= 1'b0;
        end
    end
endmodule

module axi_checked_rw_mem
#(
    parameter int size = 256,
    parameter int adr_limit = 64,
    parameter int read_ready_phase = 2,
    parameter int aw_ready_phase = 1,
    parameter int w_ready_phase = 3
)(
    input  logic        clk,
    input  logic        areset,

    input  logic [0:0]  arid,
    input  logic [31:0] araddr,
    input  logic [7:0]  arlen,
    input  logic [2:0]  arsize,
    input  logic [1:0]  arburst,
    input  logic        arlock,
    input  logic [3:0]  arcache,
    input  logic [2:0]  arprot,
    input  logic [3:0]  arqos,
    input  logic [3:0]  arregion,
    input  logic [0:0]  aruser,
    input  logic        arvalid,
    output logic        arready,

    output logic [0:0]  rid,
    output logic [31:0] rdata,
    output logic [1:0]  rresp,
    output logic        rlast,
    output logic [0:0]  ruser,
    output logic        rvalid,
    input  logic        rready,

    input  logic [0:0]  awid,
    input  logic [31:0] awaddr,
    input  logic [7:0]  awlen,
    input  logic [2:0]  awsize,
    input  logic [1:0]  awburst,
    input  logic        awlock,
    input  logic [3:0]  awcache,
    input  logic [2:0]  awprot,
    input  logic [3:0]  awqos,
    input  logic [3:0]  awregion,
    input  logic [0:0]  awuser,
    input  logic        awvalid,
    output logic        awready,

    input  logic [31:0] wdata,
    input  logic [3:0]  wstrb,
    input  logic        wlast,
    input  logic [0:0]  wuser,
    input  logic        wvalid,
    output logic        wready,

    output logic [0:0]  bid,
    output logic [1:0]  bresp,
    output logic [0:0]  buser,
    output logic        bvalid,
    input  logic        bready,

    input  logic        stop_enable,
    output bit          stop_active,
    input  logic        verbose
);

    localparam int ADDR_W = $clog2(size);

    bit [31:0] sram[size];

    logic [31:0] cycle = 32'd0;
    logic [31:0] rd_req_count = 32'd0;
    logic [31:0] wr_req_count = 32'd0;

    logic        rd_pending = 1'b0;
    logic [3:0]  rd_delay = 4'd0;
    logic [0:0]  rid_q = 1'b0;
    logic [31:0] rdata_q = 32'd0;

    logic        have_aw = 1'b0;
    logic        have_w = 1'b0;
    logic        b_pending = 1'b0;
    logic [3:0]  b_delay = 4'd0;
    logic [0:0]  bid_q = 1'b0;
    logic [31:0] awaddr_q = 32'd0;
    logic [31:0] wdata_q = 32'd0;

    logic        hold_ar = 1'b0;
    logic        hold_aw = 1'b0;
    logic        hold_w = 1'b0;
    logic [62:0] held_ar;
    logic [62:0] held_aw;
    logic [37:0] held_w;
    wire  [62:0] ar_payload = {arid, araddr, arlen, arsize, arburst, arlock,
                               arcache, arprot, arqos, arregion, aruser};
    wire  [62:0] aw_payload = {awid, awaddr, awlen, awsize, awburst, awlock,
                               awcache, awprot, awqos, awregion, awuser};
    wire  [37:0] w_payload = {wdata, wstrb, wlast, wuser};
    wire  [ADDR_W-1:0] araddr_word = araddr[ADDR_W+1:2];
    wire  [ADDR_W-1:0] awaddr_word_q = awaddr_q[ADDR_W+1:2];
    wire               write_active = have_aw || have_w || b_pending;

    assign arready = !rd_pending && !write_active && ((cycle + read_ready_phase) % 4 != 1);
    assign rid     = rid_q;
    assign rdata   = rdata_q;
    assign rresp   = 2'b00;
    assign rlast   = 1'b1;
    assign ruser   = 1'b0;
    assign rvalid  = rd_pending && rd_delay == 0;

    assign awready = !have_aw && !b_pending && ((cycle + aw_ready_phase) % 4 != 1);
    assign wready  = !have_w && !b_pending && ((cycle + w_ready_phase) % 4 != 2);
    assign bid     = bid_q;
    assign bresp   = 2'b00;
    assign buser   = 1'b0;
    assign bvalid  = b_pending && b_delay == 0;

    task automatic fail(input string msg);
        begin
            $display("FAIL: %m: %s", msg);
            stop_active = 1'b1;
            if (stop_enable)
                $fatal(1);
            else
                $fatal(1);
        end
    endtask

    always_ff @(posedge clk) begin
        cycle <= cycle + 1;
        stop_active = 1'b0;

        if (areset) begin
            rd_pending <= 1'b0;
            rd_delay <= 4'd0;
            have_aw <= 1'b0;
            have_w <= 1'b0;
            b_pending <= 1'b0;
            b_delay <= 4'd0;
            hold_ar <= 1'b0;
            hold_aw <= 1'b0;
            hold_w <= 1'b0;
        end else begin
        if (arvalid && !arready) begin
            if (hold_ar && held_ar != ar_payload)
                fail("AR payload changed while stalled");
            hold_ar <= 1'b1;
            held_ar <= ar_payload;
        end else begin
            hold_ar <= 1'b0;
        end

        if (awvalid && !awready) begin
            if (hold_aw && held_aw != aw_payload)
                fail("AW payload changed while stalled");
            hold_aw <= 1'b1;
            held_aw <= aw_payload;
        end else begin
            hold_aw <= 1'b0;
        end

        if (wvalid && !wready) begin
            if (hold_w && held_w != w_payload)
                fail("W payload changed while stalled");
            hold_w <= 1'b1;
            held_w <= w_payload;
        end else begin
            hold_w <= 1'b0;
        end

        if (rd_pending && arvalid)
            fail("new AR issued before read response");
        if (write_active && arvalid)
            fail("data read issued before write response");
        if (have_aw && awvalid)
            fail("new AW issued before write response");
        if (have_w && wvalid)
            fail("new W issued before write response");

        if (arvalid && arready) begin
            if (arlen != 8'd0)
                fail("ARLEN is not single beat");
            if (arsize != 3'd2)
                fail("ARSIZE is not 32-bit");
            if (arburst != 2'b01)
                fail("ARBURST is not INCR");
            if (araddr[1:0] != 2'b00)
                fail("unaligned read address");
            if (araddr > adr_limit * 4)
                fail("read address out of limit");

            rd_pending <= 1'b1;
            rd_delay <= rd_req_count[0] ? 4'd2 : 4'd3;
            rd_req_count <= rd_req_count + 1;
            rid_q <= arid;
            rdata_q <= sram[araddr_word];
            if (verbose)
                $display("[%m] AXI read accepted address %08x data %08x", araddr, sram[araddr_word]);
        end else if (rd_pending && rd_delay != 0) begin
            rd_delay <= rd_delay - 1;
        end

        if (rvalid && rready)
            rd_pending <= 1'b0;

        if (awvalid && awready) begin
            if (awlen != 8'd0)
                fail("AWLEN is not single beat");
            if (awsize != 3'd2)
                fail("AWSIZE is not 32-bit");
            if (awburst != 2'b01)
                fail("AWBURST is not INCR");
            if (awaddr[1:0] != 2'b00)
                fail("unaligned write address");
            if (awaddr > adr_limit * 4)
                fail("write address out of limit");

            have_aw <= 1'b1;
            bid_q <= awid;
            awaddr_q <= awaddr;
            if (verbose)
                $display("[%m] AXI write address accepted %08x", awaddr);
        end

        if (wvalid && wready) begin
            if (wstrb != 4'b1111)
                fail("WSTRB is not full word");
            if (!wlast)
                fail("WLAST is not asserted");

            have_w <= 1'b1;
            wdata_q <= wdata;
            if (verbose)
                $display("[%m] AXI write data accepted %08x", wdata);
        end

        if (have_aw && have_w && !b_pending) begin
            sram[awaddr_word_q] <= wdata_q;
            have_aw <= 1'b0;
            have_w <= 1'b0;
            b_pending <= 1'b1;
            b_delay <= wr_req_count[0] ? 4'd2 : 4'd4;
            wr_req_count <= wr_req_count + 1;
            if (verbose)
                $display("[%m] AXI write committed address %08x data %08x", awaddr_q, wdata_q);
        end else if (b_pending && b_delay != 0) begin
            b_delay <= b_delay - 1;
        end

        if (bvalid && bready)
            b_pending <= 1'b0;
        end
    end
endmodule

`default_nettype wire
