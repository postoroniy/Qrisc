import Qrisc_pack::*;
import AXI4_Types :: *;
import RegFile :: * ;
import FIFOF :: *;
import Vector :: *;
import Semi_FIFOF::*;
import Qrisc32 :: *;

import Connectable  :: *;

module mkIsaTestbench(Empty);

    Reg#(Bit#(32)) cycle_count <- mkReg(0);

    RegFile#(Bit#(32), Bit#(32)) imem <- mkRegFileLoad("../tests/isa.hex", 0, 328);
    RegFile#(Bit#(32), Bit#(32)) dmem <- mkRegFileLoad("../tests/dmem_zero.hex", 0, 27);

    AXI4_Slave_Xactor_IFC#(1,32,32,1) axi_inst <- mkAXI4_Slave_Xactor_2;
    AXI4_Slave_Xactor_IFC#(1,32,32,1) axi_data <- mkAXI4_Slave_Xactor_2;

    Qrisc_if#(1,32,32,1) cpu <- qrisc32;
    mkConnection(cpu.axi_instruction, axi_inst.axi_side);
    mkConnection(cpu.axi_data, axi_data.axi_side);

    Reg#(Bool) checking <- mkReg(False);
    Reg#(Bit#(5)) check_idx <- mkReg(0);

    Reg#(Bool) inst_rd_pending <- mkReg(False);
    Reg#(Bit#(32)) inst_rd_addr <- mkReg(0);
    Reg#(Bit#(8)) inst_rd_beats_left <- mkReg(0);
    Reg#(Bit#(1)) inst_rd_id <- mkReg(0);

    rule inst_accept_req(!inst_rd_pending && axi_inst.o_rd_addr.notEmpty);
        let req = axi_inst.o_rd_addr.first;
        axi_inst.o_rd_addr.deq;
        inst_rd_pending <= True;
        inst_rd_addr <= truncate(req.araddr);
        inst_rd_beats_left <= req.arlen + 1;
        inst_rd_id <= req.arid;
    endrule

    rule inst_send_beat(inst_rd_pending && axi_inst.i_rd_data.notFull);
        Bit#(32) data = imem.sub(inst_rd_addr >> 2);
        Bool last = (inst_rd_beats_left == 1);
        axi_inst.i_rd_data.enq(AXI4_Rd_Data {rid: inst_rd_id, rdata: data, rresp: axi4_resp_okay, rlast: last, ruser: 0});
        inst_rd_addr <= inst_rd_addr + 4;
        inst_rd_beats_left <= inst_rd_beats_left - 1;
        if (last) inst_rd_pending <= False;
    endrule

    Reg#(Bool) data_rd_pending <- mkReg(False);
    Reg#(Bit#(32)) data_rd_addr <- mkReg(0);
    Reg#(Bit#(8)) data_rd_beats_left <- mkReg(0);
    Reg#(Bit#(1)) data_rd_id <- mkReg(0);

    rule data_accept_rd_req(!data_rd_pending && axi_data.o_rd_addr.notEmpty);
        let req = axi_data.o_rd_addr.first;
        axi_data.o_rd_addr.deq;
        data_rd_pending <= True;
        data_rd_addr <= truncate(req.araddr);
        data_rd_beats_left <= req.arlen + 1;
        data_rd_id <= req.arid;
    endrule

    rule data_send_rd_beat(data_rd_pending && axi_data.i_rd_data.notFull);
        Bit#(32) data = dmem.sub(data_rd_addr >> 2);
        Bool last = (data_rd_beats_left == 1);
        axi_data.i_rd_data.enq(AXI4_Rd_Data {rid: data_rd_id, rdata: data, rresp: axi4_resp_okay, rlast: last, ruser: 0});
        data_rd_addr <= data_rd_addr + 4;
        data_rd_beats_left <= data_rd_beats_left - 1;
        if (last) data_rd_pending <= False;
    endrule

    Reg#(Bool) have_aw <- mkReg(False);
    Reg#(AXI4_Wr_Addr#(1, 32, 1)) aw <- mkRegU;
    Reg#(Bit#(32)) wr_addr <- mkReg(0);
    Reg#(Bit#(8)) wr_beats_left <- mkReg(0);

    rule capture_aw(!have_aw && axi_data.o_wr_addr.notEmpty);
        let req = axi_data.o_wr_addr.first;
        axi_data.o_wr_addr.deq;
        aw <= req;
        have_aw <= True;
        wr_addr <= truncate(req.awaddr);
        wr_beats_left <= req.awlen + 1;
    endrule

    rule data_write(have_aw && axi_data.o_wr_data.notEmpty);
        let wd = axi_data.o_wr_data.first;
        axi_data.o_wr_data.deq;
        dmem.upd(wr_addr >> 2, wd.wdata);
        wr_addr <= wr_addr + 4;

        Bool last = (wr_beats_left == 1) || wd.wlast;
        wr_beats_left <= wr_beats_left - 1;

        if (last) begin
            axi_data.i_wr_resp.enq(AXI4_Wr_Resp {bid: aw.awid, bresp: axi4_resp_okay, buser: 0});
            have_aw <= False;
        end
    endrule

    function Bit#(32) expected(Bit#(5) idx);
        case (idx)
            0: return 0;
            1: return 1;
            2: return 0;
            3: return 0;
            4: return 0;
            5: return 1;
            6: return 0;
            7: return 1;
            8: return 0;
            9: return 0;
            10: return 0;
            11: return 1;
            12: return 1;
            13: return 0;
            14: return 0;
            15: return 1;
            16: return 0;
            17: return 1;
            18: return 1;
            19: return 0;
            20: return 0;
            21: return 1;
            22: return 1;
            23: return 0;
            24: return 1;
            25: return 0;
            26: return 0;
            default: return 1;
        endcase
    endfunction

    rule tick;
        cycle_count <= cycle_count + 1;
        if (cycle_count == 3000) begin
            checking <= True;
            check_idx <= 0;
        end
    endrule

    rule check_word(checking);
        Bit#(32) got = dmem.sub(zeroExtend(check_idx));
        Bit#(32) exp = expected(check_idx);
        if (got != exp) begin
            $display("FAIL: dmem[%0d]=%0d expected %0d", check_idx, got, exp);
            $fatal(1);
        end
        else if (check_idx == 27) begin
            $display("PASS: ISA parity vector");
            $finish(0);
        end
        else begin
            check_idx <= check_idx + 1;
        end
    endrule
endmodule
