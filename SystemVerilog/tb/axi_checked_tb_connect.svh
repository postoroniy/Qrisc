  logic [0:0]  i_arid;
  logic [31:0] i_araddr;
  logic [7:0]  i_arlen;
  logic [2:0]  i_arsize;
  logic [1:0]  i_arburst;
  logic        i_arlock;
  logic [3:0]  i_arcache;
  logic [2:0]  i_arprot;
  logic [3:0]  i_arqos;
  logic [3:0]  i_arregion;
  logic [0:0]  i_aruser;
  logic        i_arvalid;
  logic        i_arready;
  logic [0:0]  i_rid;
  logic [31:0] i_rdata;
  logic [1:0]  i_rresp;
  logic        i_rlast;
  logic [0:0]  i_ruser;
  logic        i_rvalid;
  logic        i_rready;

  logic [0:0]  d_arid;
  logic [31:0] d_araddr;
  logic [7:0]  d_arlen;
  logic [2:0]  d_arsize;
  logic [1:0]  d_arburst;
  logic        d_arlock;
  logic [3:0]  d_arcache;
  logic [2:0]  d_arprot;
  logic [3:0]  d_arqos;
  logic [3:0]  d_arregion;
  logic [0:0]  d_aruser;
  logic        d_arvalid;
  logic        d_arready;
  logic [0:0]  d_rid;
  logic [31:0] d_rdata;
  logic [1:0]  d_rresp;
  logic        d_rlast;
  logic [0:0]  d_ruser;
  logic        d_rvalid;
  logic        d_rready;

  logic [0:0]  d_awid;
  logic [31:0] d_awaddr;
  logic [7:0]  d_awlen;
  logic [2:0]  d_awsize;
  logic [1:0]  d_awburst;
  logic        d_awlock;
  logic [3:0]  d_awcache;
  logic [2:0]  d_awprot;
  logic [3:0]  d_awqos;
  logic [3:0]  d_awregion;
  logic [0:0]  d_awuser;
  logic        d_awvalid;
  logic        d_awready;
  logic [31:0] d_wdata;
  logic [3:0]  d_wstrb;
  logic        d_wlast;
  logic [0:0]  d_wuser;
  logic        d_wvalid;
  logic        d_wready;
  logic [0:0]  d_bid;
  logic [1:0]  d_bresp;
  logic [0:0]  d_buser;
  logic        d_bvalid;
  logic        d_bready;

  axi_checked_read_mem #(.size(IMEM_SIZE), .adr_limit(IMEM_ADR_LIMIT), .ready_phase(0)) Iram(
      .clk(clk),
      .areset(areset),
      .arid(i_arid),
      .araddr(i_araddr),
      .arlen(i_arlen),
      .arsize(i_arsize),
      .arburst(i_arburst),
      .arlock(i_arlock),
      .arcache(i_arcache),
      .arprot(i_arprot),
      .arqos(i_arqos),
      .arregion(i_arregion),
      .aruser(i_aruser),
      .arvalid(i_arvalid),
      .arready(i_arready),
      .rid(i_rid),
      .rdata(i_rdata),
      .rresp(i_rresp),
      .rlast(i_rlast),
      .ruser(i_ruser),
      .rvalid(i_rvalid),
      .rready(i_rready),
      .stop_enable(Istop_enable),
      .stop_active(Istop_active),
      .verbose(verbose)
  );

  axi_checked_rw_mem #(.size(DMEM_SIZE), .adr_limit(DMEM_ADR_LIMIT)) Dram(
      .clk(clk),
      .areset(areset),
      .arid(d_arid),
      .araddr(d_araddr),
      .arlen(d_arlen),
      .arsize(d_arsize),
      .arburst(d_arburst),
      .arlock(d_arlock),
      .arcache(d_arcache),
      .arprot(d_arprot),
      .arqos(d_arqos),
      .arregion(d_arregion),
      .aruser(d_aruser),
      .arvalid(d_arvalid),
      .arready(d_arready),
      .rid(d_rid),
      .rdata(d_rdata),
      .rresp(d_rresp),
      .rlast(d_rlast),
      .ruser(d_ruser),
      .rvalid(d_rvalid),
      .rready(d_rready),
      .awid(d_awid),
      .awaddr(d_awaddr),
      .awlen(d_awlen),
      .awsize(d_awsize),
      .awburst(d_awburst),
      .awlock(d_awlock),
      .awcache(d_awcache),
      .awprot(d_awprot),
      .awqos(d_awqos),
      .awregion(d_awregion),
      .awuser(d_awuser),
      .awvalid(d_awvalid),
      .awready(d_awready),
      .wdata(d_wdata),
      .wstrb(d_wstrb),
      .wlast(d_wlast),
      .wuser(d_wuser),
      .wvalid(d_wvalid),
      .wready(d_wready),
      .bid(d_bid),
      .bresp(d_bresp),
      .buser(d_buser),
      .bvalid(d_bvalid),
      .bready(d_bready),
      .stop_enable(Dstop_enable),
      .stop_active(Dstop_active),
      .verbose(verbose)
  );

  qrisc32 UUT(
      .areset(areset),
      .clk(clk),

      .axi_instruction_arid(i_arid),
      .axi_instruction_araddr(i_araddr),
      .axi_instruction_arlen(i_arlen),
      .axi_instruction_arsize(i_arsize),
      .axi_instruction_arburst(i_arburst),
      .axi_instruction_arlock(i_arlock),
      .axi_instruction_arcache(i_arcache),
      .axi_instruction_arprot(i_arprot),
      .axi_instruction_arqos(i_arqos),
      .axi_instruction_arregion(i_arregion),
      .axi_instruction_aruser(i_aruser),
      .axi_instruction_arvalid(i_arvalid),
      .axi_instruction_arready(i_arready),
      .axi_instruction_rid(i_rid),
      .axi_instruction_rdata(i_rdata),
      .axi_instruction_rresp(i_rresp),
      .axi_instruction_rlast(i_rlast),
      .axi_instruction_ruser(i_ruser),
      .axi_instruction_rvalid(i_rvalid),
      .axi_instruction_rready(i_rready),

      .axi_data_arid(d_arid),
      .axi_data_araddr(d_araddr),
      .axi_data_arlen(d_arlen),
      .axi_data_arsize(d_arsize),
      .axi_data_arburst(d_arburst),
      .axi_data_arlock(d_arlock),
      .axi_data_arcache(d_arcache),
      .axi_data_arprot(d_arprot),
      .axi_data_arqos(d_arqos),
      .axi_data_arregion(d_arregion),
      .axi_data_aruser(d_aruser),
      .axi_data_arvalid(d_arvalid),
      .axi_data_arready(d_arready),
      .axi_data_rid(d_rid),
      .axi_data_rdata(d_rdata),
      .axi_data_rresp(d_rresp),
      .axi_data_rlast(d_rlast),
      .axi_data_ruser(d_ruser),
      .axi_data_rvalid(d_rvalid),
      .axi_data_rready(d_rready),
      .axi_data_awid(d_awid),
      .axi_data_awaddr(d_awaddr),
      .axi_data_awlen(d_awlen),
      .axi_data_awsize(d_awsize),
      .axi_data_awburst(d_awburst),
      .axi_data_awlock(d_awlock),
      .axi_data_awcache(d_awcache),
      .axi_data_awprot(d_awprot),
      .axi_data_awqos(d_awqos),
      .axi_data_awregion(d_awregion),
      .axi_data_awuser(d_awuser),
      .axi_data_awvalid(d_awvalid),
      .axi_data_awready(d_awready),
      .axi_data_wdata(d_wdata),
      .axi_data_wstrb(d_wstrb),
      .axi_data_wlast(d_wlast),
      .axi_data_wuser(d_wuser),
      .axi_data_wvalid(d_wvalid),
      .axi_data_wready(d_wready),
      .axi_data_bid(d_bid),
      .axi_data_bresp(d_bresp),
      .axi_data_buser(d_buser),
      .axi_data_bvalid(d_bvalid),
      .axi_data_bready(d_bready),
      .verbose(verbose)
  );
