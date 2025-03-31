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
import Qrisc_pack::*;
import AXI4_Types :: *;
import FIFOF :: *;
import Vector :: *;
import Semi_FIFOF::*;

module qrisc32_IF#(
    numeric wd_id,
    numeric wd_addr,
    numeric wd_data,
    numeric wd_user,
    FIFOF#(Instruction) instructionFifo)
    (
        IF_ifc#(wd_id, wd_addr, wd_data, wd_user)
    )provisos (
        Bits#(Bit#(32), wd_data),
        Bits#(Bit#(32), wd_addr)
);

    AXI4_Master_Xactor_IFC#(wd_id, wd_addr, wd_data, wd_user) master_instruction_if <- mkAXI4_Master_Xactor_2;

    Reg#(Word32)  address_r <- mkRegA(unpack(0));

    rule feedInstructionFifo(instructionFifo.notFull);
        let request = AXI4_Rd_Addr {
            arid: 0,
            araddr: address_r,
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
        master_instruction_if.i_rd_addr.enq(request);

        let instr = master_instruction_if.o_rd_data.first;
        master_instruction_if.o_rd_data.deq;
        let i = Instruction {
            data : instr.rdata ,
            address : address_r
        };
        instructionFifo.enq(i);
    endrule

    method Action set_new_address(Word32 new_address);
        address_r <= new_address;
    endmethod
    interface axi_instruction = master_instruction_if.axi_side;
endmodule
