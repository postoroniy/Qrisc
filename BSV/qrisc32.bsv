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
import AXI4_Types :: *;
import FIFOF :: *;
import Vector :: *;
import Semi_FIFOF::*;

import Qrisc_pack::*;
import qrisc32_IF::*;
import qrisc32_ID::*;
import qrisc32_EX::*;
import qrisc32_MEM::*;

module qrisc32(Qrisc_if #(1, 32,32, 1));

    FIFOF#(Instruction) instructionFifo <- mkFIFOF;
    FIFOF#(Pipe_s) decodeFifo <- mkFIFOF;
    FIFOF#(Pipe_s) exeFifo <- mkFIFOF;
    FIFOF#(Pipe_s) wbExFifo <- mkFIFOF;
    FIFOF#(Pipe_s) wbMemFifo <- mkFIFOF;

    IF_ifc#(1, 32, 32, 1) instructionFetchStage <- qrisc32_IF(1, 32, 32, 1,instructionFifo);
    Empty instructionDecodeStage <- qrisc32_ID(instructionFifo,decodeFifo, wbExFifo,wbMemFifo);
    EX_ifc instructionExecuteStage <- qrisc32_EX(decodeFifo, exeFifo, wbExFifo);
    MEM_ifc#(1, 32, 32, 1) memStage <- qrisc32_MEM(1, 32, 32, 1, exeFifo, wbMemFifo);

    rule update_address;
        let new_address <- instructionExecuteStage.get_new_address();
        instructionFetchStage.set_new_address(new_address);
    endrule

    interface axi_data = memStage.axi_data;
    interface axi_instruction  =  instructionFetchStage.axi_instruction;
endmodule
