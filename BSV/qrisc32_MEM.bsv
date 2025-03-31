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

    rule memIO(exeFifo.notEmpty);
        let pipe_mem_in = exeFifo.first;
        exeFifo.deq;

        if(pipe_mem_in.read_mem) begin
            let request = AXI4_Rd_Addr {
                arid: 0,
                araddr: pack(pipe_mem_in.val_r1),
                arlen: 4,
                arsize: 2,
                arburst: axburst_incr,
                arlock: 0,
                arcache: 0,
                arprot: 0,
                arqos: 0,
                arregion: 0,
                aruser: 0
            };
            master_data_if.i_rd_addr.enq(request);

            let data = master_data_if.o_rd_data.first;
            pipe_mem_in.val_dst = unpack(data.rdata);
            pipe_mem_in.write_reg = True;
            wbMemFifo.enq(pipe_mem_in);
        end
        else begin
            let write_addr = AXI4_Wr_Addr {
                awid: 0,
                awaddr: pack(pipe_mem_in.val_r1),
                awlen: 4,
                awsize: 2,
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
                wdata   :  pack(pipe_mem_in.val_dst),
                wuser   : 0,
                wlast   : True,
                wstrb   : 1
            };
            master_data_if.i_wr_data.enq(write_data);
        end
    endrule
    interface axi_data = master_data_if.axi_side;
endmodule

// module [Module] mkQrisc32_MEM#(FIFOF#(Pipe_s) exeFifo, FIFOF#(Pipe_s) wbMemFifo) (MEM_ifc#(1, 32, 32, 1));
//     MEM_ifc#(1, 32, 32, 1) memStage <- qrisc32_MEM(1, 32, 32, 1, exeFifo, wbMemFifo);
//     return memStage;
// endmodule
