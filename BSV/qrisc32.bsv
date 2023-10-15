//////////////////////////////////////////////////////////////////////////////////////////////
//    Project Qrisc32 is risc cpu implementation, purpose is studying
//    Digital System Design course at Kyoung Hee University during my PhD earning
//    Copyright (C) 2010-2023  Viacheslav Vinogradov
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
//with 3 avalon interfaces:
// - Data Read Avalon interface
// - Data Write Avalon interface
// - Instruction Read Avalon interface
//--------------------------------------------------------------------------------------------
import Qrisc_pack::*;
import qrisc32_IF::*;
import qrisc32_ID::*;
// import qrisc32_EX::*;
// import qrisc32_MEM::*;

module qrisc32(Qrisc32Ifc);
    // input  logic       verbose//for simulation
    // Wire#(Bool) test     <- mkBypassWire(b.verbose);

    // PipeIfc   pipe_id_out,//I decode
    //             pipe_ex_out,//Ex
    //             pipe_mem_out;//MEM access
    //
    // wire[31:0]  instruction,pc;
    // wire        new_address_valid_ex;
    // wire[31:0]  new_address_ex;
    //
    // wire        new_address_valid_mem;
    // wire[31:0]  new_address_mem;
    //
    // wire        pipe_stall;
    //
    // let qIf_mod <- qrisc32_IF;
    IF_ifc qIF  <- qrisc32_IF;
    ID_ifc qID  <- qrisc32_ID;

    interface avm_instructions = qIF.avm_instructions;


    // interface ifo = qIF.ifo;

    //     .pipe_stall(pipe_stall),
    //     .avm_instructions(avm_instructions),
    //     .new_address_valid(new_address_valid_ex),
    //     .new_address(new_address_ex),
    //
    //     .instruction(instruction),
    //     .pc(pc)
    // );
    //
    // qrisc32_ID  qrisc32_ID(
    //     .pipe_stall(pipe_stall),
    //     .instruction(instruction),
    //     .pc(pc),
    //     .pipe_wb_mem(pipe_mem_out),//for memory read
    //     .pipe_wb_ex(pipe_ex_out),//for R2 register and ALU operations only
    //     .pipe_id_out(pipe_id_out),
    //     .verbose(verbose)
    // );
    //
    // qrisc32_EX  qrisc32_EX(
    //     .pipe_stall(pipe_stall),
    //     .pipe_ex_in(pipe_id_out),
    //     .pipe_ex_out(pipe_ex_out),
    //     .new_address_valid(new_address_valid_ex),
    //     .new_address(new_address_ex)
    // );
    //
    // qrisc32_MEM qrisc32_MEM(
    //     .pipe_mem_in(pipe_ex_out),
    //     .avm_data_read(avm_data_read),
    //     .avm_data_write(avm_data_write),
    //     .pipe_mem_out(pipe_mem_out),
    //     .pipe_stall(pipe_stall),
    //     .verbose(verbose)
    // );

endmodule
