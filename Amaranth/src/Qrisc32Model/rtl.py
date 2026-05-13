from __future__ import annotations

from amaranth import Array, Cat, ClockDomain, ClockSignal, Const, Elaboratable, Module, Mux, Signal


PIPE_FIELDS = (
    ("val_r1", 32),
    ("val_r2", 32),
    ("val_dst", 32),
    ("src_r2", 5),
    ("src_r1", 5),
    ("dst_r", 5),
    ("incr_r2", 4),
    ("incr_r2_enable", 1),
    ("write_reg", 1),
    ("read_mem", 1),
    ("write_mem", 1),
    ("and_op", 1),
    ("or_op", 1),
    ("xor_op", 1),
    ("add_op", 1),
    ("mul_op", 1),
    ("cmp_op", 1),
    ("ldrf_op", 1),
    ("shl_op", 1),
    ("shr_op", 1),
    ("jmpunc", 1),
    ("jmpz", 1),
    ("jmpnz", 1),
    ("jmpc", 1),
    ("jmpnc", 1),
)


def make_pipe(prefix: str) -> dict[str, Signal]:
    return {name: Signal(width, name=f"{prefix}_{name}") for name, width in PIPE_FIELDS}


def pipe_eq(dst: dict[str, Signal], src: dict[str, Signal]):
    return [dst[name].eq(src[name]) for name, _ in PIPE_FIELDS]


def pipe_zero(dst: dict[str, Signal]):
    return [dst[name].eq(0) for name, _ in PIPE_FIELDS]


class Qrisc32(Elaboratable):
    def __init__(self):
        self.clk = Signal(name="clk")
        self.areset = Signal(name="areset")

        self.axi_instruction_arid = Signal(1, name="axi_instruction_arid")
        self.axi_instruction_araddr = Signal(32, name="axi_instruction_araddr")
        self.axi_instruction_arlen = Signal(8, name="axi_instruction_arlen")
        self.axi_instruction_arsize = Signal(3, name="axi_instruction_arsize")
        self.axi_instruction_arburst = Signal(2, name="axi_instruction_arburst")
        self.axi_instruction_arlock = Signal(name="axi_instruction_arlock")
        self.axi_instruction_arcache = Signal(4, name="axi_instruction_arcache")
        self.axi_instruction_arprot = Signal(3, name="axi_instruction_arprot")
        self.axi_instruction_arqos = Signal(4, name="axi_instruction_arqos")
        self.axi_instruction_arregion = Signal(4, name="axi_instruction_arregion")
        self.axi_instruction_aruser = Signal(1, name="axi_instruction_aruser")
        self.axi_instruction_arvalid = Signal(name="axi_instruction_arvalid")
        self.axi_instruction_arready = Signal(name="axi_instruction_arready")

        self.axi_instruction_rid = Signal(1, name="axi_instruction_rid")
        self.axi_instruction_rdata = Signal(32, name="axi_instruction_rdata")
        self.axi_instruction_rresp = Signal(2, name="axi_instruction_rresp")
        self.axi_instruction_rlast = Signal(name="axi_instruction_rlast")
        self.axi_instruction_ruser = Signal(1, name="axi_instruction_ruser")
        self.axi_instruction_rvalid = Signal(name="axi_instruction_rvalid")
        self.axi_instruction_rready = Signal(name="axi_instruction_rready")

        self.axi_data_arid = Signal(1, name="axi_data_arid")
        self.axi_data_araddr = Signal(32, name="axi_data_araddr")
        self.axi_data_arlen = Signal(8, name="axi_data_arlen")
        self.axi_data_arsize = Signal(3, name="axi_data_arsize")
        self.axi_data_arburst = Signal(2, name="axi_data_arburst")
        self.axi_data_arlock = Signal(name="axi_data_arlock")
        self.axi_data_arcache = Signal(4, name="axi_data_arcache")
        self.axi_data_arprot = Signal(3, name="axi_data_arprot")
        self.axi_data_arqos = Signal(4, name="axi_data_arqos")
        self.axi_data_arregion = Signal(4, name="axi_data_arregion")
        self.axi_data_aruser = Signal(1, name="axi_data_aruser")
        self.axi_data_arvalid = Signal(name="axi_data_arvalid")
        self.axi_data_arready = Signal(name="axi_data_arready")

        self.axi_data_rid = Signal(1, name="axi_data_rid")
        self.axi_data_rdata = Signal(32, name="axi_data_rdata")
        self.axi_data_rresp = Signal(2, name="axi_data_rresp")
        self.axi_data_rlast = Signal(name="axi_data_rlast")
        self.axi_data_ruser = Signal(1, name="axi_data_ruser")
        self.axi_data_rvalid = Signal(name="axi_data_rvalid")
        self.axi_data_rready = Signal(name="axi_data_rready")

        self.axi_data_awid = Signal(1, name="axi_data_awid")
        self.axi_data_awaddr = Signal(32, name="axi_data_awaddr")
        self.axi_data_awlen = Signal(8, name="axi_data_awlen")
        self.axi_data_awsize = Signal(3, name="axi_data_awsize")
        self.axi_data_awburst = Signal(2, name="axi_data_awburst")
        self.axi_data_awlock = Signal(name="axi_data_awlock")
        self.axi_data_awcache = Signal(4, name="axi_data_awcache")
        self.axi_data_awprot = Signal(3, name="axi_data_awprot")
        self.axi_data_awqos = Signal(4, name="axi_data_awqos")
        self.axi_data_awregion = Signal(4, name="axi_data_awregion")
        self.axi_data_awuser = Signal(1, name="axi_data_awuser")
        self.axi_data_awvalid = Signal(name="axi_data_awvalid")
        self.axi_data_awready = Signal(name="axi_data_awready")

        self.axi_data_wdata = Signal(32, name="axi_data_wdata")
        self.axi_data_wstrb = Signal(4, name="axi_data_wstrb")
        self.axi_data_wlast = Signal(name="axi_data_wlast")
        self.axi_data_wuser = Signal(1, name="axi_data_wuser")
        self.axi_data_wvalid = Signal(name="axi_data_wvalid")
        self.axi_data_wready = Signal(name="axi_data_wready")

        self.axi_data_bid = Signal(1, name="axi_data_bid")
        self.axi_data_bresp = Signal(2, name="axi_data_bresp")
        self.axi_data_buser = Signal(1, name="axi_data_buser")
        self.axi_data_bvalid = Signal(name="axi_data_bvalid")
        self.axi_data_bready = Signal(name="axi_data_bready")

        self.verbose = Signal(name="verbose")

    def ports(self):
        return [
            self.clk,
            self.areset,
            self.axi_instruction_arid,
            self.axi_instruction_araddr,
            self.axi_instruction_arlen,
            self.axi_instruction_arsize,
            self.axi_instruction_arburst,
            self.axi_instruction_arlock,
            self.axi_instruction_arcache,
            self.axi_instruction_arprot,
            self.axi_instruction_arqos,
            self.axi_instruction_arregion,
            self.axi_instruction_aruser,
            self.axi_instruction_arvalid,
            self.axi_instruction_arready,
            self.axi_instruction_rid,
            self.axi_instruction_rdata,
            self.axi_instruction_rresp,
            self.axi_instruction_rlast,
            self.axi_instruction_ruser,
            self.axi_instruction_rvalid,
            self.axi_instruction_rready,
            self.axi_data_arid,
            self.axi_data_araddr,
            self.axi_data_arlen,
            self.axi_data_arsize,
            self.axi_data_arburst,
            self.axi_data_arlock,
            self.axi_data_arcache,
            self.axi_data_arprot,
            self.axi_data_arqos,
            self.axi_data_arregion,
            self.axi_data_aruser,
            self.axi_data_arvalid,
            self.axi_data_arready,
            self.axi_data_rid,
            self.axi_data_rdata,
            self.axi_data_rresp,
            self.axi_data_rlast,
            self.axi_data_ruser,
            self.axi_data_rvalid,
            self.axi_data_rready,
            self.axi_data_awid,
            self.axi_data_awaddr,
            self.axi_data_awlen,
            self.axi_data_awsize,
            self.axi_data_awburst,
            self.axi_data_awlock,
            self.axi_data_awcache,
            self.axi_data_awprot,
            self.axi_data_awqos,
            self.axi_data_awregion,
            self.axi_data_awuser,
            self.axi_data_awvalid,
            self.axi_data_awready,
            self.axi_data_wdata,
            self.axi_data_wstrb,
            self.axi_data_wlast,
            self.axi_data_wuser,
            self.axi_data_wvalid,
            self.axi_data_wready,
            self.axi_data_bid,
            self.axi_data_bresp,
            self.axi_data_buser,
            self.axi_data_bvalid,
            self.axi_data_bready,
            self.verbose,
        ]

    def elaborate(self, platform):
        m = Module()
        m.domains.sync = ClockDomain(reset_less=True)
        m.d.comb += ClockSignal("sync").eq(self.clk)

        pipe_id_out = make_pipe("pipe_id_out")
        pipe_id_w = make_pipe("pipe_id_w")
        pipe_ex_out = make_pipe("pipe_ex_out")
        pipe_ex_w = make_pipe("pipe_ex_w")
        pipe_mem_out = make_pipe("pipe_mem_out")
        pipe_mem_in0 = make_pipe("pipe_mem_in0")
        pipe_mem_in1 = make_pipe("pipe_mem_in1")
        pipe_wb_ex = make_pipe("pipe_wb_ex")
        pipe_wb_mem = make_pipe("pipe_wb_mem")

        rf = Array(Signal(32, name=f"rf_{i}") for i in range(32))
        imem_addr = Signal(32, name="if_instruction_address")
        imem_data = Signal(32, name="if_instruction_data")
        imem_wait = Signal(name="if_instruction_wait")
        instruction = Signal(32, name="if_instruction")
        stalled_adr0 = Signal(32, name="if_stalled_adr0")
        stalled_adr1 = Signal(32, name="if_stalled_adr1")

        dmem_rd_addr = Signal(32, name="mem_read_address")
        dmem_rd_data = Signal(32, name="mem_read_data")
        dmem_rd = Signal(name="mem_read")
        dmem_rd_wait = Signal(name="mem_read_wait")

        dmem_wr_addr = Signal(32, name="mem_write_address")
        dmem_wr_data = Signal(32, name="mem_write_data")
        dmem_wr = Signal(name="mem_write")
        dmem_wr_wait = Signal(name="mem_write_wait")

        def read_bridge(prefix, mem_addr, mem_rd, mem_data, mem_wait, arid, araddr,
                        arlen, arsize, arburst, arlock, arcache, arprot, arqos,
                        arregion, aruser, arvalid, arready, rdata, rvalid, rready,
                        same_cycle_done):
            r_idle = Const(0, 2)
            r_addr = Const(1, 2)
            r_resp = Const(2, 2)
            state = Signal(2, name=f"{prefix}_read_state")
            addr_q = Signal(32, name=f"{prefix}_read_addr_q")
            ar_fire = Signal(name=f"{prefix}_ar_fire")
            read_done = Signal(name=f"{prefix}_read_done")

            m.d.comb += [
                arid.eq(0),
                araddr.eq(Mux(state == r_idle, mem_addr, addr_q)),
                arlen.eq(0),
                arsize.eq(2),
                arburst.eq(1),
                arlock.eq(0),
                arcache.eq(0),
                arprot.eq(0),
                arqos.eq(0),
                arregion.eq(0),
                aruser.eq(0),
                arvalid.eq(((state == r_idle) & mem_rd) | (state == r_addr)),
                rready.eq(1),
                ar_fire.eq(arvalid & arready),
                mem_data.eq(rdata),
                mem_wait.eq(mem_rd & ~read_done),
            ]
            if same_cycle_done:
                m.d.comb += read_done.eq(
                    ((state == r_idle) & ar_fire & rvalid)
                    | ((state == r_addr) & ar_fire & rvalid)
                    | ((state == r_resp) & rvalid)
                )
            else:
                m.d.comb += read_done.eq((state == r_resp) & rvalid)

            with m.If(self.areset):
                m.d.sync += [state.eq(r_idle), addr_q.eq(0)]
            with m.Else():
                with m.Switch(state):
                    with m.Case(r_idle):
                        with m.If(mem_rd & ~read_done):
                            m.d.sync += [
                                addr_q.eq(mem_addr),
                                state.eq(Mux(ar_fire, r_resp, r_addr)),
                            ]
                    with m.Case(r_addr):
                        with m.If(ar_fire):
                            if same_cycle_done:
                                m.d.sync += state.eq(Mux(rvalid, r_idle, r_resp))
                            else:
                                m.d.sync += state.eq(r_resp)
                    with m.Case(r_resp):
                        with m.If(rvalid):
                            m.d.sync += state.eq(r_idle)
                    with m.Default():
                        m.d.sync += state.eq(r_idle)

        read_bridge(
            "instruction",
            imem_addr,
            Const(1, 1),
            imem_data,
            imem_wait,
            self.axi_instruction_arid,
            self.axi_instruction_araddr,
            self.axi_instruction_arlen,
            self.axi_instruction_arsize,
            self.axi_instruction_arburst,
            self.axi_instruction_arlock,
            self.axi_instruction_arcache,
            self.axi_instruction_arprot,
            self.axi_instruction_arqos,
            self.axi_instruction_arregion,
            self.axi_instruction_aruser,
            self.axi_instruction_arvalid,
            self.axi_instruction_arready,
            self.axi_instruction_rdata,
            self.axi_instruction_rvalid,
            self.axi_instruction_rready,
            True,
        )

        read_bridge(
            "data",
            dmem_rd_addr,
            dmem_rd,
            dmem_rd_data,
            dmem_rd_wait,
            self.axi_data_arid,
            self.axi_data_araddr,
            self.axi_data_arlen,
            self.axi_data_arsize,
            self.axi_data_arburst,
            self.axi_data_arlock,
            self.axi_data_arcache,
            self.axi_data_arprot,
            self.axi_data_arqos,
            self.axi_data_arregion,
            self.axi_data_aruser,
            self.axi_data_arvalid,
            self.axi_data_arready,
            self.axi_data_rdata,
            self.axi_data_rvalid,
            self.axi_data_rready,
            True,
        )

        w_idle = Const(0, 2)
        w_addr_data = Const(1, 2)
        w_resp = Const(2, 2)
        w_state = Signal(2, name="data_write_state")
        aw_done = Signal(name="data_write_aw_done")
        w_done = Signal(name="data_write_w_done")
        awaddr_q = Signal(32, name="data_write_addr_q")
        wdata_q = Signal(32, name="data_write_data_q")
        aw_fire = Signal(name="data_write_aw_fire")
        w_fire = Signal(name="data_write_w_fire")
        write_done = Signal(name="data_write_done")

        m.d.comb += [
            self.axi_data_awid.eq(0),
            self.axi_data_awaddr.eq(Mux(w_state == w_idle, dmem_wr_addr, awaddr_q)),
            self.axi_data_awlen.eq(0),
            self.axi_data_awsize.eq(2),
            self.axi_data_awburst.eq(1),
            self.axi_data_awlock.eq(0),
            self.axi_data_awcache.eq(0),
            self.axi_data_awprot.eq(0),
            self.axi_data_awqos.eq(0),
            self.axi_data_awregion.eq(0),
            self.axi_data_awuser.eq(0),
            self.axi_data_awvalid.eq(((w_state == w_idle) & dmem_wr) | ((w_state == w_addr_data) & ~aw_done)),
            self.axi_data_wdata.eq(Mux(w_state == w_idle, dmem_wr_data, wdata_q)),
            self.axi_data_wstrb.eq(0xF),
            self.axi_data_wlast.eq(1),
            self.axi_data_wuser.eq(0),
            self.axi_data_wvalid.eq(((w_state == w_idle) & dmem_wr) | ((w_state == w_addr_data) & ~w_done)),
            self.axi_data_bready.eq(1),
            aw_fire.eq(self.axi_data_awvalid & self.axi_data_awready),
            w_fire.eq(self.axi_data_wvalid & self.axi_data_wready),
            write_done.eq(
                ((w_state == w_idle) & aw_fire & w_fire & self.axi_data_bvalid)
                | ((w_state == w_addr_data) & (aw_done | aw_fire) & (w_done | w_fire) & self.axi_data_bvalid)
                | ((w_state == w_resp) & self.axi_data_bvalid)
            ),
            dmem_wr_wait.eq(dmem_wr & ~write_done),
        ]

        with m.If(self.areset):
            m.d.sync += [
                w_state.eq(w_idle),
                aw_done.eq(0),
                w_done.eq(0),
                awaddr_q.eq(0),
                wdata_q.eq(0),
            ]
        with m.Else():
            with m.Switch(w_state):
                with m.Case(w_idle):
                    m.d.sync += [aw_done.eq(0), w_done.eq(0)]
                    with m.If(dmem_wr):
                        m.d.sync += [awaddr_q.eq(dmem_wr_addr), wdata_q.eq(dmem_wr_data)]
                        with m.If(~write_done):
                            m.d.sync += [
                                aw_done.eq(aw_fire),
                                w_done.eq(w_fire),
                                w_state.eq(Mux(aw_fire & w_fire, w_resp, w_addr_data)),
                            ]
                with m.Case(w_addr_data):
                    with m.If(aw_fire):
                        m.d.sync += aw_done.eq(1)
                    with m.If(w_fire):
                        m.d.sync += w_done.eq(1)
                    with m.If((aw_done | aw_fire) & (w_done | w_fire)):
                        m.d.sync += w_state.eq(Mux(self.axi_data_bvalid, w_idle, w_resp))
                with m.Case(w_resp):
                    with m.If(self.axi_data_bvalid):
                        m.d.sync += w_state.eq(w_idle)
                with m.Default():
                    m.d.sync += w_state.eq(w_idle)

        pc_if = imem_addr
        new_address_valid = Signal(name="ex_new_address_valid")
        new_address = Signal(32, name="ex_new_address")
        pipe_stall = Signal(name="pipe_stall")
        if_stall = Signal(name="if_stall")
        if_base = Signal(32, name="if_base")
        if_offset = Signal(32, name="if_offset")
        if_next_addr = Signal(32, name="if_next_addr")

        m.d.comb += [
            if_stall.eq(imem_wait | pipe_stall),
            if_base.eq(imem_addr),
            if_offset.eq(4),
        ]
        with m.If(if_stall):
            m.d.comb += if_offset.eq(0)
        with m.Elif(new_address_valid):
            m.d.comb += [if_base.eq(new_address), if_offset.eq(0)]
        m.d.comb += if_next_addr.eq(if_base + if_offset)

        inst = instruction
        dst = inst[0:5]
        src1 = inst[5:10]
        src2 = inst[10:15]
        imm16 = inst[5:21]
        off15 = inst[10:25]
        offset_w = Signal(32, name="id_offset")
        id_val_r1 = Signal(32, name="id_val_r1")
        id_val_r2 = Signal(32, name="id_val_r2")
        id_val_dst = Signal(32, name="id_val_dst")
        incr = Signal(4, name="id_incr")
        incr_enable = Signal(name="id_incr_enable")

        def forwarded_value(reg_idx, name):
            value = Signal(32, name=name)
            m.d.comb += value.eq(rf[reg_idx])
            with m.If(pipe_mem_out["write_reg"] & (pipe_mem_out["dst_r"] == reg_idx)):
                m.d.comb += value.eq(pipe_mem_out["val_dst"])
            with m.Elif(pipe_ex_out["write_reg"] & (pipe_ex_out["dst_r"] == reg_idx)):
                m.d.comb += value.eq(pipe_ex_out["val_dst"])
            with m.Elif(pipe_ex_out["incr_r2_enable"] & (pipe_ex_out["src_r2"] == reg_idx)):
                m.d.comb += value.eq(pipe_ex_out["val_r2"])
            with m.Elif(pipe_wb_mem["write_reg"] & (pipe_wb_mem["dst_r"] == reg_idx)):
                m.d.comb += value.eq(pipe_wb_mem["val_dst"])
            with m.Elif(pipe_wb_ex["write_reg"] & (pipe_wb_ex["dst_r"] == reg_idx)):
                m.d.comb += value.eq(pipe_wb_ex["val_dst"])
            with m.Elif(pipe_wb_ex["incr_r2_enable"] & (pipe_wb_ex["src_r2"] == reg_idx)):
                m.d.comb += value.eq(pipe_wb_ex["val_r2"])
            return value

        m.d.comb += pipe_zero(pipe_id_w)
        m.d.comb += [
            id_val_r1.eq(forwarded_value(src1, "id_forward_r1")),
            id_val_r2.eq(forwarded_value(src2, "id_forward_r2")),
            id_val_dst.eq(forwarded_value(dst, "id_forward_dst")),
            offset_w.eq(Mux(inst[25], id_val_r2, Cat(off15, off15[14].replicate(17)))),
            incr.eq(0),
            incr_enable.eq(0),
            pipe_id_w["dst_r"].eq(dst),
            pipe_id_w["src_r1"].eq(src1),
            pipe_id_w["src_r2"].eq(src2),
            pipe_id_w["val_r1"].eq(id_val_r1),
            pipe_id_w["val_r2"].eq(id_val_r2),
            pipe_id_w["val_dst"].eq(id_val_dst),
        ]

        with m.Switch(inst[22:25]):
            with m.Case(1):
                m.d.comb += [incr.eq(1), incr_enable.eq(1)]
            with m.Case(2):
                m.d.comb += [incr.eq(2), incr_enable.eq(1)]
            with m.Case(3):
                m.d.comb += [incr.eq(4), incr_enable.eq(1)]
            with m.Case(5):
                m.d.comb += [incr.eq(Const(-1, 4)), incr_enable.eq(1)]
            with m.Case(6):
                m.d.comb += [incr.eq(Const(-2, 4)), incr_enable.eq(1)]
            with m.Case(7):
                m.d.comb += [incr.eq(Const(-4, 4)), incr_enable.eq(1)]

        m.d.comb += [
            pipe_id_w["incr_r2"].eq(incr),
            pipe_id_w["incr_r2_enable"].eq(incr_enable),
        ]

        with m.Switch(inst[28:32]):
            with m.Case(0):
                with m.Switch(inst[26:28]):
                    with m.Case(0):
                        m.d.comb += [
                            pipe_id_w["write_reg"].eq(dst != src1),
                            pipe_id_w["val_dst"].eq(id_val_r1),
                        ]
                    with m.Case(1):
                        m.d.comb += [
                            pipe_id_w["val_dst"].eq(Cat(id_val_dst[0:16], imm16)),
                            pipe_id_w["write_reg"].eq(1),
                            pipe_id_w["incr_r2_enable"].eq(0),
                        ]
                    with m.Case(2):
                        m.d.comb += [
                            pipe_id_w["val_dst"].eq(Cat(imm16, id_val_dst[16:32])),
                            pipe_id_w["write_reg"].eq(1),
                            pipe_id_w["incr_r2_enable"].eq(0),
                        ]
                    with m.Default():
                        m.d.comb += [
                            pipe_id_w["read_mem"].eq(1),
                            pipe_id_w["write_reg"].eq(1),
                            pipe_id_w["incr_r2_enable"].eq(inst[25]),
                            pipe_id_w["val_r2"].eq(offset_w),
                        ]
            with m.Case(1):
                m.d.comb += [
                    pipe_id_w["write_mem"].eq(1),
                    pipe_id_w["val_r2"].eq(offset_w),
                    pipe_id_w["incr_r2_enable"].eq(inst[25]),
                ]
            with m.Case(2):
                with m.Switch(inst[26:28]):
                    with m.Case(0):
                        m.d.comb += [
                            pipe_id_w["val_r1"].eq(inst[0:26]),
                            pipe_id_w["val_r2"].eq(0),
                            pipe_id_w["incr_r2_enable"].eq(0),
                            pipe_id_w["jmpunc"].eq(1),
                        ]
                    with m.Case(1):
                        m.d.comb += [
                            pipe_id_w["val_r1"].eq(pc_if),
                            pipe_id_w["val_r2"].eq(offset_w),
                            pipe_id_w["incr_r2_enable"].eq(inst[25]),
                            pipe_id_w["jmpunc"].eq(1),
                        ]
                    with m.Case(2):
                        m.d.comb += [
                            pipe_id_w["val_r1"].eq(pc_if),
                            pipe_id_w["val_r2"].eq(offset_w),
                            pipe_id_w["val_dst"].eq(pc_if),
                            pipe_id_w["incr_r2_enable"].eq(inst[25]),
                            pipe_id_w["jmpunc"].eq(1),
                            pipe_id_w["write_reg"].eq(1),
                        ]
                    with m.Default():
                        m.d.comb += [
                            pipe_id_w["val_r1"].eq(id_val_dst),
                            pipe_id_w["val_r2"].eq(0),
                            pipe_id_w["incr_r2_enable"].eq(inst[25]),
                            pipe_id_w["jmpunc"].eq(1),
                        ]
            with m.Case(3):
                m.d.comb += [
                    pipe_id_w["val_r1"].eq(pc_if),
                    pipe_id_w["val_r2"].eq(offset_w),
                    pipe_id_w["incr_r2_enable"].eq(inst[25]),
                ]
                with m.Switch(inst[26:28]):
                    with m.Case(0):
                        m.d.comb += pipe_id_w["jmpz"].eq(1)
                    with m.Case(1):
                        m.d.comb += pipe_id_w["jmpnz"].eq(1)
                    with m.Case(2):
                        m.d.comb += pipe_id_w["jmpc"].eq(1)
                    with m.Default():
                        m.d.comb += pipe_id_w["jmpnc"].eq(1)
            with m.Case(4):
                m.d.comb += pipe_id_w["write_reg"].eq(inst[25:28] != 7)
                with m.Switch(inst[25:28]):
                    with m.Case(0):
                        m.d.comb += pipe_id_w["and_op"].eq(1)
                    with m.Case(1):
                        m.d.comb += pipe_id_w["or_op"].eq(1)
                    with m.Case(2):
                        m.d.comb += pipe_id_w["xor_op"].eq(1)
                    with m.Case(3):
                        m.d.comb += pipe_id_w["add_op"].eq(1)
                    with m.Case(4):
                        m.d.comb += pipe_id_w["mul_op"].eq(1)
                    with m.Case(5):
                        m.d.comb += pipe_id_w["shl_op"].eq(1)
                    with m.Case(6):
                        m.d.comb += pipe_id_w["shr_op"].eq(1)
                    with m.Default():
                        m.d.comb += [pipe_id_w["cmp_op"].eq(1), pipe_id_w["write_reg"].eq(0)]
            with m.Case(5):
                m.d.comb += [pipe_id_w["ldrf_op"].eq(1), pipe_id_w["write_reg"].eq(1)]
                with m.Switch(inst[26:28]):
                    with m.Case(0):
                        m.d.comb += pipe_id_w["jmpz"].eq(1)
                    with m.Case(1):
                        m.d.comb += pipe_id_w["jmpnz"].eq(1)
                    with m.Case(2):
                        m.d.comb += pipe_id_w["jmpc"].eq(1)
                    with m.Default():
                        m.d.comb += pipe_id_w["jmpnc"].eq(1)

        flag_z = Signal(name="ex_flag_z")
        flag_c = Signal(name="ex_flag_c")
        flag_z_w = Signal(name="ex_flag_z_w")
        flag_c_w = Signal(name="ex_flag_c_w")
        r2_add = Signal(32, name="ex_r2_add")
        sum_result = Signal(33, name="ex_sum_result")
        prod_result = Signal(64, name="ex_prod_result")
        shl_result = Signal(64, name="ex_shl_result")
        shr_result = Signal(33, name="ex_shr_result")

        m.d.comb += pipe_eq(pipe_ex_w, pipe_id_out)
        sum_wide = Cat(pipe_id_out["val_r1"], Const(0, 1)) + Cat(pipe_id_out["val_r2"], Const(0, 1))
        shl_wide = Cat(pipe_id_out["val_r1"], Const(0, 32)) << pipe_id_out["val_r2"][0:5]
        m.d.comb += [
            flag_z_w.eq(0),
            flag_c_w.eq(0),
            r2_add.eq(pipe_id_out["val_r2"] + Cat(pipe_id_out["incr_r2"], pipe_id_out["incr_r2"][3].replicate(28))),
            sum_result.eq(sum_wide[0:33]),
            prod_result.eq(pipe_id_out["val_r1"] * pipe_id_out["val_r2"]),
            shl_result.eq(shl_wide),
            shr_result.eq(Cat(Const(0, 1), pipe_id_out["val_r1"]) >> pipe_id_out["val_r2"][0:5]),
        ]

        with m.If(pipe_id_out["ldrf_op"]):
            with m.If(
                (pipe_id_out["jmpz"] & flag_z)
                | (pipe_id_out["jmpnz"] & ~flag_z)
                | (pipe_id_out["jmpc"] & flag_c)
                | (pipe_id_out["jmpnc"] & ~flag_c)
            ):
                m.d.comb += pipe_ex_w["val_dst"].eq(pipe_id_out["val_r1"])
            with m.Else():
                m.d.comb += pipe_ex_w["val_dst"].eq(pipe_id_out["val_r2"])
        with m.Elif(pipe_id_out["and_op"]):
            m.d.comb += [
                pipe_ex_w["val_dst"].eq(pipe_id_out["val_r1"] & pipe_id_out["val_r2"]),
                flag_c_w.eq(0),
                flag_z_w.eq(pipe_ex_w["val_dst"] == 0),
            ]
        with m.Elif(pipe_id_out["or_op"]):
            m.d.comb += [
                pipe_ex_w["val_dst"].eq(pipe_id_out["val_r1"] | pipe_id_out["val_r2"]),
                flag_c_w.eq(0),
                flag_z_w.eq(pipe_ex_w["val_dst"] == 0),
            ]
        with m.Elif(pipe_id_out["xor_op"]):
            m.d.comb += [
                pipe_ex_w["val_dst"].eq(pipe_id_out["val_r1"] ^ pipe_id_out["val_r2"]),
                flag_c_w.eq(0),
                flag_z_w.eq(pipe_ex_w["val_dst"] == 0),
            ]
        with m.Elif(pipe_id_out["add_op"] | pipe_id_out["jmpunc"] | pipe_id_out["jmpz"] | pipe_id_out["jmpnz"] | pipe_id_out["jmpc"] | pipe_id_out["jmpnc"]):
            m.d.comb += [
                pipe_ex_w["val_dst"].eq(sum_result[0:32]),
                flag_c_w.eq(sum_result[32]),
                flag_z_w.eq(sum_result[0:32] == 0),
            ]
        with m.Elif(pipe_id_out["mul_op"]):
            m.d.comb += [
                pipe_ex_w["val_dst"].eq(prod_result[0:32]),
                flag_c_w.eq(prod_result[32]),
                flag_z_w.eq(prod_result[0:32] == 0),
            ]
        with m.Elif(pipe_id_out["shl_op"]):
            m.d.comb += [
                pipe_ex_w["val_dst"].eq(shl_result[0:32]),
                flag_c_w.eq(shl_result[32]),
                flag_z_w.eq(shl_result[0:32] == 0),
            ]
        with m.Elif(pipe_id_out["shr_op"]):
            m.d.comb += [
                pipe_ex_w["val_dst"].eq(shr_result[1:33]),
                flag_c_w.eq(shr_result[0]),
                flag_z_w.eq(shr_result[1:33] == 0),
            ]
        with m.Elif(pipe_id_out["cmp_op"]):
            m.d.comb += [
                flag_z_w.eq(pipe_id_out["val_r1"] == pipe_id_out["val_r2"]),
                flag_c_w.eq(pipe_id_out["val_r1"] < pipe_id_out["val_r2"]),
            ]

        with m.If(pipe_id_out["incr_r2_enable"]):
            m.d.comb += pipe_ex_w["val_r2"].eq(r2_add)

        m.d.comb += [
            dmem_rd_addr.eq(pipe_ex_out["val_r1"]),
            dmem_rd.eq(pipe_ex_out["read_mem"]),
            dmem_wr_addr.eq(pipe_ex_out["val_r1"]),
            dmem_wr_data.eq(pipe_ex_out["val_dst"]),
            dmem_wr.eq(pipe_ex_out["write_mem"]),
            pipe_stall.eq((pipe_ex_out["read_mem"] & dmem_rd_wait) | (pipe_ex_out["write_mem"] & dmem_wr_wait)),
        ]

        with m.If(self.areset):
            m.d.sync += pipe_zero(pipe_id_out)
            m.d.sync += pipe_zero(pipe_ex_out)
            m.d.sync += pipe_zero(pipe_mem_out)
            m.d.sync += pipe_zero(pipe_mem_in0)
            m.d.sync += pipe_zero(pipe_mem_in1)
            m.d.sync += pipe_zero(pipe_wb_ex)
            m.d.sync += pipe_zero(pipe_wb_mem)
            m.d.sync += [
                imem_addr.eq(0),
                instruction.eq(0),
                stalled_adr0.eq(0),
                stalled_adr1.eq(0),
                flag_z.eq(0),
                flag_c.eq(0),
                new_address_valid.eq(0),
                new_address.eq(0),
            ]
            for reg in rf:
                m.d.sync += reg.eq(0)
        with m.Else():
            with m.If(~if_stall):
                m.d.sync += [
                    instruction.eq(imem_data),
                    imem_addr.eq(if_next_addr),
                    stalled_adr0.eq(if_next_addr),
                    stalled_adr1.eq(stalled_adr0),
                ]
            with m.Else():
                m.d.sync += imem_addr.eq(stalled_adr0)

            with m.If(~pipe_stall):
                m.d.sync += pipe_eq(pipe_id_out, pipe_id_w)

            with m.If(pipe_wb_ex["write_reg"]):
                m.d.sync += rf[pipe_wb_ex["dst_r"]].eq(pipe_wb_ex["val_dst"])
            with m.If(pipe_wb_ex["incr_r2_enable"]):
                m.d.sync += rf[pipe_wb_ex["src_r2"]].eq(pipe_wb_ex["val_r2"])
            with m.If(pipe_wb_mem["write_reg"]):
                m.d.sync += rf[pipe_wb_mem["dst_r"]].eq(pipe_wb_mem["val_dst"])

            with m.If(
                pipe_id_out["and_op"]
                | pipe_id_out["or_op"]
                | pipe_id_out["xor_op"]
                | pipe_id_out["add_op"]
                | pipe_id_out["mul_op"]
                | pipe_id_out["shl_op"]
                | pipe_id_out["shr_op"]
                | pipe_id_out["cmp_op"]
            ):
                m.d.sync += [flag_z.eq(flag_z_w), flag_c.eq(flag_c_w)]

            with m.If(
                pipe_id_out["jmpunc"]
                | (pipe_id_out["jmpz"] & flag_z)
                | (pipe_id_out["jmpnz"] & ~flag_z)
                | (pipe_id_out["jmpc"] & flag_c)
                | (pipe_id_out["jmpnc"] & ~flag_c)
            ):
                m.d.sync += [
                    new_address_valid.eq(~pipe_id_out["ldrf_op"]),
                    new_address.eq(pipe_ex_w["val_dst"]),
                ]
            with m.Else():
                m.d.sync += new_address_valid.eq(0)

            with m.If(~pipe_stall):
                m.d.sync += pipe_eq(pipe_ex_out, pipe_ex_w)
                with m.If(pipe_id_out["read_mem"] | pipe_id_out["write_mem"]):
                    m.d.sync += pipe_ex_out["val_r1"].eq(sum_result[0:32])

            m.d.sync += pipe_eq(pipe_mem_in0, pipe_ex_out)
            m.d.sync += pipe_eq(pipe_mem_in1, pipe_mem_in0)

            with m.If(pipe_mem_in1["read_mem"] & ~dmem_rd_wait):
                m.d.sync += pipe_eq(pipe_mem_out, pipe_ex_out)
                m.d.sync += [
                    pipe_mem_out["dst_r"].eq(pipe_mem_in1["dst_r"]),
                    pipe_mem_out["val_dst"].eq(dmem_rd_data),
                    pipe_mem_out["write_reg"].eq(1),
                ]
            with m.Elif(~pipe_stall):
                m.d.sync += pipe_eq(pipe_mem_out, pipe_ex_out)
                with m.If(pipe_ex_out["read_mem"]):
                    m.d.sync += pipe_mem_out["write_reg"].eq(0)

            m.d.sync += pipe_eq(pipe_wb_ex, pipe_ex_out)
            m.d.sync += pipe_eq(pipe_wb_mem, pipe_mem_out)

        return m
