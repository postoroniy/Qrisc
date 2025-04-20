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
import AXI4_Types :: *;
import FIFOF :: *;
import Vector :: *;
import Semi_FIFOF::*;

import Qrisc_pack::*;

module qrisc32_MEM#(
    numeric wd_id,
    numeric wd_addr,
    numeric wd_data,
    numeric wd_user,
    FIFOF#(Pipe_s) exeFifo,
    FIFOF#(Pipe_s) wbMemFifo)
    (MEM_ifc#(wd_id, wd_addr, wd_data, wd_user))
    provisos (
        Bits#(Bit#(32), wd_data),
        Bits#(Bit#(32), wd_addr)
    );

    AXI4_Master_Xactor_IFC#(wd_id, wd_addr, wd_data, wd_user) master_data_if <- mkAXI4_Master_Xactor_2;

    Reg#(MaybePipe_s) pending <- mkRegA(Invalid);
    Reg#(Bool)        pending_is_read <- mkRegA(False);

    rule start_req(isValid(pending) == False && exeFifo.notEmpty);
        let p = exeFifo.first;

        if (p.read_mem) begin
            if (master_data_if.i_rd_addr.notFull) begin
                let request = AXI4_Rd_Addr {
                    arid: 0,
                    araddr: p.val_r1,
                    arlen: 0,
                    arsize: axsize_4,
                    arburst: axburst_incr,
                    arlock: 0,
                    arcache: 0,
                    arprot: 0,
                    arqos: 0,
                    arregion: 0,
                    aruser: 0
                };
                master_data_if.i_rd_addr.enq(request);
                pending <= tagged Valid p;
                pending_is_read <= True;
                exeFifo.deq;
            end
        end
        else if (p.write_mem) begin
            if (master_data_if.i_wr_addr.notFull && master_data_if.i_wr_data.notFull) begin
                let write_addr = AXI4_Wr_Addr {
                    awid: 0,
                    awaddr: p.val_r1,
                    awlen: 0,
                    awsize: axsize_4,
                    awburst: axburst_incr,
                    awlock: 0,
                    awcache: 0,
                    awprot: 0,
                    awqos: 0,
                    awregion: 0,
                    awuser: 0
                };
                master_data_if.i_wr_addr.enq(write_addr);

                let write_data = AXI4_Wr_Data {
                    wdata: p.val_dst,
                    wuser: 0,
                    wlast: True,
                    wstrb: 4'b1111
                };
                master_data_if.i_wr_data.enq(write_data);

                pending <= tagged Valid p;
                pending_is_read <= False;
                exeFifo.deq;
            end
        end
        else begin
            if (wbMemFifo.notFull) begin
                wbMemFifo.enq(p);
                exeFifo.deq;
            end
        end
    endrule

    rule finish_read(
        isValid(pending) &&
        pending_is_read &&
        master_data_if.o_rd_data.notEmpty &&
        wbMemFifo.notFull
    );
        let p = validValue(pending);
        let resp = master_data_if.o_rd_data.first;
        master_data_if.o_rd_data.deq;
        p.val_dst = resp.rdata;
        p.write_reg = True;
        wbMemFifo.enq(p);
        pending <= Invalid;
    endrule

    rule finish_write(
        isValid(pending) &&
        !pending_is_read &&
        master_data_if.o_wr_resp.notEmpty &&
        wbMemFifo.notFull
    );
        let p = validValue(pending);
        master_data_if.o_wr_resp.deq;
        wbMemFifo.enq(p);
        pending <= Invalid;
    endrule

    method Bool can_accept_cmd();
        return isValid(pending) == False &&
            master_data_if.i_rd_addr.notFull &&
            master_data_if.i_wr_addr.notFull &&
            master_data_if.i_wr_data.notFull;
    endmethod

    method Bool can_accept_rd_resp();
        return True;
    endmethod

    method Bool can_accept_wr_resp();
        return True;
    endmethod

    method Bool is_busy();
        return isValid(pending);
    endmethod

    interface axi_data = master_data_if.axi_side;
endmodule
