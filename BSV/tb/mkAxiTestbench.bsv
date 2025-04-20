import Qrisc_pack::*;
import AXI4_Types :: *;
import RegFile :: * ;
import FIFOF :: *;
import Vector :: *;
import Semi_FIFOF::*;
import qrisc32 :: *;

import Connectable  :: *;

module mkAxiTestbench(Empty);

    Reg#(Bit#(32)) cycle_count <- mkReg(0);

    RegFile#(Bit#(32), Bit#(32)) imem <- mkRegFileLoad("build/hex/axi_smoke_1024.hex", 0, 1023);
    RegFile#(Bit#(32), Bit#(32)) dmem <- mkRegFile(0, 1023);

    AXI4_Slave_Xactor_IFC#(1,32,32,1) axi_inst <- mkAXI4_Slave_Xactor_2;
    AXI4_Slave_Xactor_IFC#(1,32,32,1) axi_data <- mkAXI4_Slave_Xactor_2;

    Qrisc_if#(1,32,32,1) cpu <- qrisc32;
    mkConnection(cpu.axi_instruction, axi_inst.axi_side);
    mkConnection(cpu.axi_data, axi_data.axi_side);

    Reg#(Bit#(32)) inst_req_count <- mkReg(0);
    Reg#(Bool) inst_rd_pending <- mkReg(False);
    Reg#(Bit#(32)) inst_rd_addr <- mkReg(0);
    Reg#(Bit#(8)) inst_rd_beats_left <- mkReg(0);
    Reg#(Bit#(4)) inst_rd_delay <- mkReg(0);
    Reg#(Bit#(1)) inst_rd_id <- mkReg(0);

    rule inst_accept_req(!inst_rd_pending && axi_inst.o_rd_addr.notEmpty && cycle_count[1:0] != 2'b01);
        let req = axi_inst.o_rd_addr.first;
        if (req.arlen != 0 || req.arsize != axsize_4 || req.arburst != axburst_incr || req.araddr[1:0] != 0) begin
            $display("FAIL: instruction AXI AR field mismatch");
            $fatal(1);
        end
        if (req.araddr > 32'd512) begin
            $display("FAIL: instruction AXI read address out of limit %08x", req.araddr);
            $fatal(1);
        end
        axi_inst.o_rd_addr.deq;
        inst_rd_pending <= True;
        inst_rd_addr <= truncate(req.araddr);
        inst_rd_beats_left <= req.arlen + 1;
        inst_rd_delay <= inst_req_count[0] == 0 ? 4'd4 : 4'd2;
        inst_req_count <= inst_req_count + 1;
        inst_rd_id <= req.arid;
    endrule

    rule inst_delay(inst_rd_pending && inst_rd_delay != 0);
        inst_rd_delay <= inst_rd_delay - 1;
    endrule

    rule inst_send_beat(inst_rd_pending && inst_rd_delay == 0 && axi_inst.i_rd_data.notFull);
        Bit#(32) data = imem.sub(inst_rd_addr >> 2);
        Bool last = (inst_rd_beats_left == 1);
        axi_inst.i_rd_data.enq(AXI4_Rd_Data {rid: inst_rd_id, rdata: data, rresp: axi4_resp_okay, rlast: last, ruser: 0});
        inst_rd_addr <= inst_rd_addr + 4;
        inst_rd_beats_left <= inst_rd_beats_left - 1;
        if (last) inst_rd_pending <= False;
    endrule

    Reg#(Bit#(32)) data_rd_req_count <- mkReg(0);
    Reg#(Bool) data_rd_pending <- mkReg(False);
    Reg#(Bit#(32)) data_rd_addr <- mkReg(0);
    Reg#(Bit#(8)) data_rd_beats_left <- mkReg(0);
    Reg#(Bit#(4)) data_rd_delay <- mkReg(0);
    Reg#(Bit#(1)) data_rd_id <- mkReg(0);

    Reg#(Bool) have_aw <- mkReg(False);
    Reg#(AXI4_Wr_Addr#(1, 32, 1)) aw <- mkRegU;
    Reg#(Bool) have_w <- mkReg(False);
    Reg#(Bit#(32)) wr_data <- mkReg(0);
    Reg#(Bool) wr_resp_pending <- mkReg(False);
    Reg#(Bit#(4)) wr_resp_delay <- mkReg(0);
    Reg#(Bit#(32)) wr_req_count <- mkReg(0);

    rule fail_data_read_during_write((have_aw || have_w || wr_resp_pending) && axi_data.o_rd_addr.notEmpty);
        $display("FAIL: data AXI read issued before write response");
        $fatal(1);
    endrule

    rule data_accept_rd_req(
        !data_rd_pending &&
        !have_aw &&
        !have_w &&
        !wr_resp_pending &&
        axi_data.o_rd_addr.notEmpty &&
        cycle_count[2:0] != 3'b010
    );
        let req = axi_data.o_rd_addr.first;
        if (req.arlen != 0 || req.arsize != axsize_4 || req.arburst != axburst_incr || req.araddr[1:0] != 0) begin
            $display("FAIL: data AXI AR field mismatch");
            $fatal(1);
        end
        if (req.araddr > 32'd4092) begin
            $display("FAIL: data AXI read address out of limit %08x", req.araddr);
            $fatal(1);
        end
        axi_data.o_rd_addr.deq;
        data_rd_pending <= True;
        data_rd_addr <= truncate(req.araddr);
        data_rd_beats_left <= req.arlen + 1;
        data_rd_delay <= data_rd_req_count[0] == 0 ? 4'd3 : 4'd2;
        data_rd_req_count <= data_rd_req_count + 1;
        data_rd_id <= req.arid;
    endrule

    rule data_delay(data_rd_pending && data_rd_delay != 0);
        data_rd_delay <= data_rd_delay - 1;
    endrule

    rule data_send_rd_beat(data_rd_pending && data_rd_delay == 0 && axi_data.i_rd_data.notFull);
        Bit#(32) data = dmem.sub(data_rd_addr >> 2);
        Bool last = (data_rd_beats_left == 1);
        axi_data.i_rd_data.enq(AXI4_Rd_Data {rid: data_rd_id, rdata: data, rresp: axi4_resp_okay, rlast: last, ruser: 0});
        data_rd_addr <= data_rd_addr + 4;
        data_rd_beats_left <= data_rd_beats_left - 1;
        if (last) data_rd_pending <= False;
    endrule

    rule fail_extra_aw((have_aw || wr_resp_pending) && axi_data.o_wr_addr.notEmpty);
        $display("FAIL: new data AXI AW issued before write response");
        $fatal(1);
    endrule

    rule fail_extra_w((have_w || wr_resp_pending) && axi_data.o_wr_data.notEmpty);
        $display("FAIL: new data AXI W issued before write response");
        $fatal(1);
    endrule

    rule capture_aw(!have_aw && !wr_resp_pending && axi_data.o_wr_addr.notEmpty && cycle_count[1:0] != 2'b10);
        let req = axi_data.o_wr_addr.first;
        if (req.awlen != 0 || req.awsize != axsize_4 || req.awburst != axburst_incr || req.awaddr[1:0] != 0) begin
            $display("FAIL: data AXI AW field mismatch");
            $fatal(1);
        end
        if (req.awaddr > 32'd4092) begin
            $display("FAIL: data AXI write address out of limit %08x", req.awaddr);
            $fatal(1);
        end
        axi_data.o_wr_addr.deq;
        aw <= req;
        have_aw <= True;
    endrule

    rule capture_w(!have_w && !wr_resp_pending && axi_data.o_wr_data.notEmpty && cycle_count[1:0] != 2'b11);
        let wd = axi_data.o_wr_data.first;
        if (wd.wstrb != 4'b1111 || !wd.wlast) begin
            $display("FAIL: data AXI W field mismatch");
            $fatal(1);
        end
        axi_data.o_wr_data.deq;
        wr_data <= wd.wdata;
        have_w <= True;
    endrule

    rule commit_write(have_aw && have_w && !wr_resp_pending);
        dmem.upd(aw.awaddr >> 2, wr_data);
        have_aw <= False;
        have_w <= False;
        wr_resp_pending <= True;
        wr_resp_delay <= wr_req_count[0] == 0 ? 4'd4 : 4'd2;
        wr_req_count <= wr_req_count + 1;
    endrule

    rule delay_write_resp(wr_resp_pending && wr_resp_delay != 0);
        wr_resp_delay <= wr_resp_delay - 1;
    endrule

    rule send_write_resp(wr_resp_pending && wr_resp_delay == 0 && axi_data.i_wr_resp.notFull);
        axi_data.i_wr_resp.enq(AXI4_Wr_Resp {bid: aw.awid, bresp: axi4_resp_okay, buser: 0});
        wr_resp_pending <= False;
    endrule

    rule tick;
        cycle_count <= cycle_count + 1;
        if (cycle_count == 32'd1000) begin
            Bit#(32) got = dmem.sub(0);
            if (got == 32'd12) begin
                $display("PASS: AXI smoke dmem[0] = %0d", got);
            end
            else begin
                $display("FAIL: AXI smoke dmem[0] = %0d (expected 12)", got);
                $fatal(1);
            end
            $finish(0);
        end
    endrule
endmodule
