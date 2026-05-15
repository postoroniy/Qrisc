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

typedef enum {
    OpLdr        = 4'd0,
    OpStr        = 4'd1,
    OpJmpUnc     = 4'd2,
    OpJmpF       = 4'd3,
    OpAlu        = 4'd4,
    OpLdrF       = 4'd5,
    OpReserved6  = 4'd6,
    OpReserved7  = 4'd7,
    OpReserved8  = 4'd8,
    OpReserved9  = 4'd9,
    OpReserved10 = 4'd10,
    OpReserved11 = 4'd11,
    OpReserved12 = 4'd12,
    OpReserved13 = 4'd13,
    OpReserved14 = 4'd14,
    OpReserved15 = 4'd15
} Opcode deriving (Bits, Eq, FShow);

typedef enum {
    LoadReg  = 2'd0,
    LoadHigh = 2'd1,
    LoadLow  = 2'd2,
    LoadMem  = 2'd3
} LoadMode deriving (Bits, Eq, FShow);

typedef enum {
    JumpAbs  = 2'd0,
    JumpRel  = 2'd1,
    JumpCall = 2'd2,
    JumpRet  = 2'd3
} JumpMode deriving (Bits, Eq, FShow);

typedef enum {
    FlagZ  = 2'd0,
    FlagNZ = 2'd1,
    FlagC  = 2'd2,
    FlagNC = 2'd3
} FlagMode deriving (Bits, Eq, FShow);

typedef enum {
    AluAnd = 3'd0,
    AluOr  = 3'd1,
    AluXor = 3'd2,
    AluAdd = 3'd3,
    AluMul = 3'd4,
    AluShl = 3'd5,
    AluShr = 3'd6,
    AluCmp = 3'd7
} AluOp deriving (Bits, Eq, FShow);

typedef enum {
    OffsetCode = 1'b0,
    OffsetReg  = 1'b1
} OffsetMode deriving (Bits, Eq, FShow);

typedef enum {
    IncrNone    = 3'd0,
    IncrOne     = 3'd1,
    IncrTwo     = 3'd2,
    IncrFour    = 3'd3,
    IncrNoneAlt = 3'd4,
    DecrOne     = 3'd5,
    DecrTwo     = 3'd6,
    DecrFour    = 3'd7
} IncrMode deriving (Bits, Eq, FShow);

Opcode op_ldr    = OpLdr;
Opcode op_str    = OpStr;
Opcode op_jmpunc = OpJmpUnc;
Opcode op_jmpf   = OpJmpF;
Opcode op_alu    = OpAlu;
Opcode op_ldrf   = OpLdrF;

OffsetMode offset_code = OffsetCode;
OffsetMode offset_r    = OffsetReg;

IncrMode incr_0 = IncrNone;
IncrMode decr_0 = IncrNone;
IncrMode incr_1 = IncrOne;
IncrMode incr_2 = IncrTwo;
IncrMode incr_4 = IncrFour;
IncrMode decr_1 = DecrOne;
IncrMode decr_2 = DecrTwo;
IncrMode decr_4 = DecrFour;

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
