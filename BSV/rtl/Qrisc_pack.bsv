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

package Qrisc_pack;

import AXI4_Types :: *;
import FIFOF :: *;
import Vector :: *;
import Semi_FIFOF::*;

typedef Bit#(32) Word32;
typedef Bit#(5)  RegIdx;

typedef struct {
    Word32  val_r1;
    Word32  val_r2;
    Word32  val_dst;

    RegIdx  src_r2;
    RegIdx  src_r1;
    RegIdx  dst_r;

    Int#(4) incr_r2;
    Bool    incr_r2_enable;

    Bool    write_reg;
    Bool    read_mem;
    Bool    write_mem;

    Bool    and_op;
    Bool    or_op;
    Bool    xor_op;
    Bool    add_op;
    Bool    mul_op;
    Bool    cmp_op;

    Bool    ldrf_op;

    Bool    shl_op;
    Bool    shr_op;

    Bool    jmpunc;
    Bool    jmpz;
    Bool    jmpnz;
    Bool    jmpc;
    Bool    jmpnc;
} Pipe_s deriving (Bits, Eq, FShow, DefaultValue);

typedef Maybe#(Pipe_s) MaybePipe_s;

Bit#(4) op_ldr    = 4'd0;
Bit#(4) op_str    = 4'd1;
Bit#(4) op_jmpunc = 4'd2;
Bit#(4) op_jmpf   = 4'd3;
Bit#(4) op_alu    = 4'd4;
Bit#(4) op_ldrf   = 4'd5;

Bit#(1) offset_code = 1'b0;
Bit#(1) offset_r    = 1'b1;

Bit#(3) incr_0 = 3'b000;
Bit#(3) decr_0 = 3'b000;
Bit#(3) incr_1 = 3'b001;
Bit#(3) incr_2 = 3'b010;
Bit#(3) incr_4 = 3'b011;
Bit#(3) decr_1 = 3'b101;
Bit#(3) decr_2 = 3'b110;
Bit#(3) decr_4 = 3'b111;

RegIdx r0  = 5'd0;
RegIdx r1  = 5'd1;
RegIdx r2  = 5'd2;
RegIdx r3  = 5'd3;
RegIdx r4  = 5'd4;
RegIdx r5  = 5'd5;
RegIdx r6  = 5'd6;
RegIdx r7  = 5'd7;
RegIdx r8  = 5'd8;
RegIdx r9  = 5'd9;
RegIdx r10 = 5'd10;
RegIdx r11 = 5'd11;
RegIdx r12 = 5'd12;
RegIdx r13 = 5'd13;
RegIdx r14 = 5'd14;
RegIdx r15 = 5'd15;
RegIdx r16 = 5'd16;
RegIdx r17 = 5'd17;
RegIdx r18 = 5'd18;
RegIdx r19 = 5'd19;
RegIdx r20 = 5'd20;
RegIdx r21 = 5'd21;
RegIdx r22 = 5'd22;
RegIdx r23 = 5'd23;
RegIdx r24 = 5'd24;
RegIdx r25 = 5'd25;
RegIdx r26 = 5'd26;
RegIdx r27 = 5'd27;
RegIdx r28 = 5'd28;
RegIdx r29 = 5'd29;
RegIdx r30 = 5'd30;
RegIdx r31 = 5'd31;

function Word32 sext15(Bit#(15) imm);
    Int#(15) imm15 = unpack(imm);
    Int#(32) imm32 = extend(imm15);
    return pack(imm32);
endfunction

function Word32 zext26(Bit#(26) imm);
    return zeroExtend(imm);
endfunction

interface Qrisc_if #(numeric type wd_id,
                    numeric type wd_addr,
                    numeric type wd_data,
                    numeric type wd_user);
    interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi_instruction;
    interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi_data;
endinterface

typedef struct {
    Word32 address;
    Word32 data;
}Instruction deriving (Bits, Eq, FShow, DefaultValue);

typedef Maybe#(Word32) MaybeWord32;
typedef Maybe#(Instruction) MaybeInstruction;

(* always_ready, always_enabled *)
interface IF_ifc#(type wd_id, type wd_addr, type wd_data, type wd_user);
    interface AXI4_Master_IFC#(wd_id, wd_addr, wd_data, wd_user) axi_instruction;
    method Action set_new_address(Word32 new_address);
endinterface

// (* always_ready, always_enabled *)
// interface ID_ifc;
// endinterface

(* always_ready, always_enabled *)
interface EX_ifc;
  method MaybeWord32 get_new_address();
endinterface

(* always_ready, always_enabled *)
interface MEM_ifc #(numeric type wd_id,
                    numeric type wd_addr,
                    numeric type wd_data,
                    numeric type wd_user);
    interface AXI4_Master_IFC#(wd_id, wd_addr, wd_data, wd_user)  axi_data;
    method Bool is_busy();
    method Bool can_accept_cmd();
    method Bool can_accept_rd_resp();
    method Bool can_accept_wr_resp();
endinterface

(* always_ready, always_enabled *)
interface RF_IFC;
    method Word32 read_r1(RegIdx addr);
    method Word32 read_r2(RegIdx addr);
    method Word32 read_dst(RegIdx addr);
    method Action write_wb(Pipe_s ex, Pipe_s mem);
endinterface

(* always_ready, always_enabled *)
interface WB_IFC;
    method Pipe_s forward_ex();
    method Pipe_s forward_mem();
endinterface
endpackage
