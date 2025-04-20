//////////////////////////////////////////////////////////////////////////////////////////////
// Simple single-beat AXI4 memory models for qrisc32 testbenches.
//////////////////////////////////////////////////////////////////////////////////////////////
`default_nettype none
`timescale 1ns / 1ns

module axi_read_mem
#(parameter size = 256, parameter adr_limit = 64)(
    input  logic        clk,

    input  logic [31:0] araddr,
    input  logic        arvalid,
    output logic        arready,

    output logic [0:0]  rid,
    output bit   [31:0] rdata,
    output logic [1:0]  rresp,
    output logic        rlast,
    output logic        rvalid,
    input  logic        rready,
    output logic [0:0]  ruser,

    input  logic        stop_enable,
    output bit          stop_active,
    input  logic        verbose
);

    localparam int ADDR_W = $clog2(size);

    bit [31:0] sram[size];

    wire [ADDR_W-1:0] araddr_word = araddr[ADDR_W+1:2];

    assign arready = 1'b1;
    assign rid     = 1'b0;
    assign rresp   = 2'b00;
    assign rlast   = 1'b1;
    assign rvalid  = 1'b1;
    assign ruser   = 1'b0;

    always_ff @(posedge clk) begin
        stop_active = 1'b0;
        if (arvalid && arready) begin
            rdata <= sram[araddr_word];
            if (verbose)
                $display(".................................[%m] AXI read at address %08x with data %08x", araddr, sram[araddr_word]);
        end

        if (arvalid && araddr > adr_limit * 4) begin
            if (verbose)
                $display(".................................[%m] AXI read out of limit address %08x", araddr);
            stop_active = 1'b1;
        end

        if (arvalid && araddr[1:0] != 0) begin
            if (verbose)
                $display(".................................[%m] AXI read by non align address %08x", araddr);
            stop_active = 1'b1;
        end

        if (stop_enable && stop_active)
            $fatal(1);
    end
endmodule

module axi_rw_mem
#(parameter size = 256, parameter adr_limit = 64)(
    input  logic        clk,

    input  logic [31:0] araddr,
    input  logic        arvalid,
    output logic        arready,

    output logic [0:0]  rid,
    output bit   [31:0] rdata,
    output logic [1:0]  rresp,
    output logic        rlast,
    output logic        rvalid,
    input  logic        rready,
    output logic [0:0]  ruser,

    input  logic [31:0] awaddr,
    input  logic        awvalid,
    output logic        awready,

    input  logic [31:0] wdata,
    input  logic [3:0]  wstrb,
    input  logic        wlast,
    input  logic        wvalid,
    output logic        wready,

    output logic [0:0]  bid,
    output logic [1:0]  bresp,
    output logic        bvalid,
    input  logic        bready,
    output logic [0:0]  buser,

    input  logic        stop_enable,
    output bit          stop_active,
    input  logic        verbose
);

    localparam int ADDR_W = $clog2(size);

    bit [31:0] sram[size];

    wire [ADDR_W-1:0] araddr_word = araddr[ADDR_W+1:2];
    wire [ADDR_W-1:0] awaddr_word = awaddr[ADDR_W+1:2];

    assign arready = 1'b1;
    assign rid     = 1'b0;
    assign rresp   = 2'b00;
    assign rlast   = 1'b1;
    assign rvalid  = 1'b1;
    assign ruser   = 1'b0;

    assign awready = 1'b1;
    assign wready  = 1'b1;
    assign bid     = 1'b0;
    assign bresp   = 2'b00;
    assign bvalid  = 1'b1;
    assign buser   = 1'b0;

    always_ff @(posedge clk) begin
        stop_active = 1'b0;
        if (awvalid && awready && wvalid && wready) begin
            if (wstrb[0]) sram[awaddr_word][7:0]   <= wdata[7:0];
            if (wstrb[1]) sram[awaddr_word][15:8]  <= wdata[15:8];
            if (wstrb[2]) sram[awaddr_word][23:16] <= wdata[23:16];
            if (wstrb[3]) sram[awaddr_word][31:24] <= wdata[31:24];
            if (verbose)
                $display(".................................[%m] AXI write at address %08x with data %08x", awaddr, wdata);
        end

        if (arvalid && arready) begin
            rdata <= sram[araddr_word];
            if (verbose)
                $display(".................................[%m] AXI read at address %08x with data %08x", araddr, sram[araddr_word]);
        end

        if (awvalid && awaddr > adr_limit * 4) begin
            if (verbose)
                $display(".................................[%m] AXI write out of limit address %08x", awaddr);
            stop_active = 1'b1;
        end

        if (arvalid && araddr > adr_limit * 4) begin
            if (verbose)
                $display(".................................[%m] AXI read out of limit address %08x", araddr);
            stop_active = 1'b1;
        end

        if (awvalid && awaddr[1:0] != 0) begin
            if (verbose)
                $display(".................................[%m] AXI write by non align address %08x", awaddr);
            stop_active = 1'b1;
        end

        if (arvalid && araddr[1:0] != 0) begin
            if (verbose)
                $display(".................................[%m] AXI read by non align address %08x", araddr);
            stop_active = 1'b1;
        end

        if (stop_enable && stop_active)
            $fatal(1);
    end
endmodule

`default_nettype wire
