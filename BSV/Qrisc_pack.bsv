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

// import

interface Qrisc_if #(numeric type wd_id,
                    numeric type wd_addr,
                    numeric type wd_data,
                    numeric type wd_user);
    interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi_instruction;
    interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi_data;
endinterface

typedef Bit#(32) Word32;
typedef struct {
    Word32 address;
    Word32 data;
}Instruction deriving (Bits, Eq, FShow, DefaultValue);

typedef Maybe#(Word32) MaybeWord32;
typedef Maybe#(Instruction) MaybeInstruction;

typedef enum {
    NoChanges,
    Incr1, Incr2, Incr4,
    Decr1, Decr2, Decr4
} IncrDecr deriving (Bits, Eq, FShow, DefaultValue);

typedef enum
  { R0 , R1 , R2 , R3 , R4 , R5 , R6 , R7 ,
    R8 , R9 , R10, R11, R12, R13, R14, R15,
    R16, R17, R18, R19, R20, R21, R22, R23,
    R24, R25, R26, R27, R28, R29, R30, R31 } Rset deriving (Bits, Eq, FShow, DefaultValue);

typedef struct {
    IncrDecr incr2;
    Rset src2;
    Rset src1;
    Rset dst;
} CommonFields deriving (Bits, Eq, FShow, DefaultValue);

typedef struct {
    Bit#(14) imData;
    Rset src1;
    Rset dst;
} OffsetCode deriving (Bits, Eq, FShow, DefaultValue);

typedef struct {
    Bit#(16) imData;
    Rset dst;
} ImmCode deriving (Bits, Eq, FShow, DefaultValue);

typedef union tagged {
    OffsetCode OffsetCode;
    CommonFields CommonFields;
} OffsetType deriving (Bits, Eq, FShow, DefaultValue);

typedef enum {
    None,
    Zero,
    NotZero,
    Carry,
    NotCarry
} CheckFlags deriving (Bits, Eq, FShow, DefaultValue);

typedef enum {
    AND,
    OR,
    XOR,
    ADD,
    MUL,
    SHL,
    SHR,
    CMP
} AluType deriving (Bits, Eq, FShow, DefaultValue);

typedef union tagged {
    struct {CheckFlags flags; CommonFields common;}     MOVF;
    struct {CheckFlags flags;ImmCode immCode;}          LDRHF;
    struct {CheckFlags flags;ImmCode immCode;}          LDRLF;
    struct {OffsetType offset;}                         LDR_Offset;
    struct {OffsetType offset;}                         STR_Offset;

    struct {CheckFlags flags; Bit#(25) imData;}         JMPF;//jmp   pc[25:0]=code[25:0] , cond or uncond depending on flags
    struct {CheckFlags flags; OffsetType offset;}       JMPRF; //pc=pc+offset(relaitive jump)    jmpr    R2(jmpr R2+4)
    struct {CheckFlags flags; OffsetType offset;}       CALLRF;//pc=pc+offset, Rdst=pc         callr    R0,0xXXXXXXX,R1+4  or callr R0,R1+4
    struct {CheckFlags flags; CommonFields common;}     RETF; //pc=Rdst                ret    Rx or ret Rx,Ry+-0,1,2,4
    struct {
        AluType alutype;
        CommonFields common;
    } ALU;
} InstructionCode deriving (Bits, Eq, FShow, DefaultValue);

// Define a constant for the NOP command
InstructionCode nop =
    tagged MOVF {
        common: CommonFields {
            incr2: NoChanges,
            src2: R0,
            src1: R0,
            dst:  R0
        },
        flags: None
    };

typedef struct {
    Bit#(32)    val_r1;//value of register src1
    Bit#(32)    val_r2;//value of register src2
    Bit#(32)    val_dst;//value of register dst

    Rset        src_r2;//indicate number of src2 register
    Rset        src_r1;//indicate number of src1 register
    Rset        dst_r;//indicate number of dest register
    //add to src2
    IncrDecr    incr2;//0 +1 or -1, +2,-2, +4, -4

    //
    Bool        write_reg;//indicate write to RF(addres in dst_r, value in dst_v)
    //load store operations, if both bits are zero then bypass MEM stage
    Bool        read_mem;//indicate read from memory(addres in src1+src2)
    Bool        write_mem;//indicate write to memory(addres in src1+src2, value in dst)

    //PC changing operations
    Bool        pc_change_op;//indicate change PC

    //alu operations
    Bool        and_op;//AND
    Bool        or_op;// OR
    Bool        xor_op;//XOR
    Bool        add_op;//+
    Bool        mul_op;//
    Bool        cmp_op;//compare operation
    //shifter operations
    Bool        shl_op;//shift left
    Bool        shr_op;//shift  right

    //check flags
    Bool        check_z;
    Bool        check_nz;
    Bool        check_c;
    Bool        check_nc;
} Pipe_s deriving (Bits, Eq, FShow, DefaultValue);

typedef Maybe#(Pipe_s) MaybePipe_s;
//Pipe_s defaultInstance = tagged Literal#(MyStruct){field1: 8'b0, field2: 16'h0};

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
  method ActionValue#(Word32) get_new_address();
endinterface

(* always_ready, always_enabled *)
interface MEM_ifc #(numeric type wd_id,
                    numeric type wd_addr,
                    numeric type wd_data,
                    numeric type wd_user);
    interface AXI4_Master_IFC#(wd_id, wd_addr, wd_data, wd_user)  axi_data;
endinterface
endpackage
