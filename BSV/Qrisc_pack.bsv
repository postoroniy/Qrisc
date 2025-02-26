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

interface Qrisc_if #(numeric type wd_id,
                    numeric type wd_addr,
                    numeric type wd_data,
                    numeric type wd_user);
    interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi_instruction;
    interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi_data;
endinterface

//Op Codes
//[31:28]
typedef enum { 
    LDR     = 4'd0,
    STR     = 4'd1,
    JMPUNC  = 4'd2,
    JMPF    = 4'd3,
    ALU     = 4'd4,
    LDRF    = 4'd5,
    Unknown = 4'd15
} Iset deriving (Bits, Eq, FShow, DefaultValue);

//[27:26]
typedef enum { 
    Mov         = 2'd0,
    Ldrh        = 2'd1,
    Ldrl        = 2'd2,
    Ldr_Offset  = 2'd3
} LdrType deriving (Bits, Eq, FShow, DefaultValue);

//[27:26]
typedef enum {
    Jmp    = 2'd0,
    Jmpr   = 2'd1,
    Callr  = 2'd2,
    Ret    = 2'd3
} JmpUncType deriving (Bits, Eq, FShow, DefaultValue);

//[27:26]
typedef enum { 
    Jmpz    = 2'd0,
    Jmpnz   = 2'd1,
    Jmpc    = 2'd2,
    Jmpnc   = 2'd3
} JmpConType deriving (Bits, Eq, FShow, DefaultValue);

//[27:26]
typedef enum { 
    Ldrz    = 2'd0,
    Ldrnz   = 2'd1,
    Ldrc    = 2'd2,
    Ldrnc   = 2'd3
} LdrfType deriving (Bits, Eq, FShow, DefaultValue);

//[27:25]
typedef enum { 
    And    = 3'd0,
    Or     = 3'd1,
    Xor    = 3'd2,
    Add    = 3'd3,
    Mul    = 3'd4,
    Shl    = 3'd5,
    Shr    = 3'd6,
    Cmp    = 3'd7
} AluType deriving (Bits, Eq, FShow, DefaultValue);

//[25]
typedef enum { 
    OffsetCode = 1'd0,
    OffsetR    = 1'd1
} OffsetType deriving (Bits, Eq, FShow, DefaultValue);

//[24:22]
typedef enum { 
    NoChanges0  = 3'd0,
    Incr1       = 3'd1,
    Incr2       = 3'd2,
    Incr4       = 3'd3,
    NoChanges1  = 3'd4,
    Decr1       = 3'd5,
    Decr2       = 3'd6,
    Decr4       = 3'd7
} IncrDecr deriving (Bits, Eq, FShow, DefaultValue);

typedef struct {
      Int#(32)    val_r1;//value of register src1
      Int#(32)    val_r2;//value of register src2
      Int#(32)    val_dst;//value of register dst

      Bit#(5)     src_r2;//indicate number of src2 register
      Bit#(5)     src_r1;//indicate number of src1 register
      Bit#(5)     dst_r;//indicate number of dest register

      //add to src2
      Int#(4)     incr_r2;//0 +1 or -1, +2,-2, +4, -4
      Bool        incr_r2_enable;//

      //
      Bool        write_reg;//indicate write to RF(addres in dst_r, value in dst_v)
      //load store operations, if both bits are zero then bypass MEM stage
      Bool        read_mem;//indicate read from memory(addres in src1+src2)
      Bool        write_mem;//indicate write to memory(addres in src1+src2, value in dst)
      //

      //if alu and shift operations are  zeros then bypass EX stage
      //alu operations
      Bool        and_op;//AND
      Bool        or_op;// OR
      Bool        xor_op;//XOR
      Bool        add_op;//+
      Bool        mul_op;//
      Bool        cmp_op;//compare operation

      Bool        ldrf_op;//conditional load
      //shifter operations
      Bool        shl_op;//shift left
      Bool        shr_op;//shift  right
      //jmp operations
      //old pc in  value in    val_r1
      //offset to  pc  value in    val_r2
      //new pc value in    val_dst
      //types of jump
      //indicate to calc new address of PC
      Bool         jmpunc;
      Bool         jmpz;
      Bool         jmpnz;
      Bool         jmpc;
      Bool         jmpnc;
} Pipe_s deriving (Bits, Eq, FShow, DefaultValue);

typedef Maybe#(Pipe_s) MaybePipe_s;

typedef Bit#(32) Word32;
typedef struct {
    Word32 address;
    Word32 data;
}Instruction deriving (Bits, Eq, FShow, DefaultValue);

typedef Maybe#(Word32) MaybeWord32;
typedef Maybe#(Instruction) MaybeInstruction;

//Pipe_s defaultInstance = tagged Literal#(MyStruct){field1: 8'b0, field2: 16'h0};

(* always_ready, always_enabled *)
interface IF_ifc#(type wd_id, type wd_addr, type wd_data, type wd_user);
    interface AXI4_Master_IFC#(wd_id, wd_addr, wd_data, wd_user) axi_instruction;
    method Action set_new_address(Word32 new_address);
endinterface

(* always_ready, always_enabled *)
interface ID_ifc;
    (* prefix = "ID" *)
    method Action wb_ex_in(Pipe_s wb_ex);
    method Action wb_mem_in(Pipe_s wb_mem);
endinterface

(* always_ready, always_enabled *)
interface EX_ifc;
  method ActionValue#(Word32) get_new_address();
endinterface

(* always_ready, always_enabled *)
interface MEM_ifc #(numeric type wd_id,
                    numeric type wd_addr,
                    numeric type wd_data,
                    numeric type wd_user);
    interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi_data;
endinterface

function Bit#(4) instr_LDR; instr_LDR= 4'd0;endfunction
//LDR Rdst,[Rsrc1],-+Rsrc2
    //[31:28]LDR_op
    //[27:26] type of LDR op
    //0- Rdst= Rsrc1
    //1-Rdst[31:16] = code[20:5]  LDRH Rx,0x1234
    //2-Rdst[15:0] = code[20:5]    LDRL Rx,0x5678
    //3- Rdst=[Rsrc1+offset]
    //[25]
    //0 - offset = code[24:10](signed)
    //1 - offset = Rsrc2(signed)
    //[24:22]
    //000  Rsrc2=Rsrc2
    //001  Rsrc2=Rsrc2+1
    //010  Rsrc2=Rsrc2+2
    //011  Rsrc2=Rsrc2+4
    //100  Rsrc2=Rsrc2
    //101  Rsrc2=Rsrc2-1
    //110  Rsrc2=Rsrc2-2
    //111  Rsrc2=Rsrc2-4
    //[14:10] src2
    //[9:5]src1
    //[4:0]dst
function Bit#(4) instr_STR; instr_STR= 4'd1;endfunction
//STR Rdst,[Rsrc1],-+Rsrc2
    //[31:28]STR_op
    //[27:26] type of STORE op
    //3- [Rsrc1+offset]=Rdst
    //[25]
    //0 - offset = code[24:10](signed)
    //1 - offset = Rsrc2(signed)
    //[24:22]
    //000  Rsrc2=Rsrc2
    //001  Rsrc2=Rsrc2+1
    //010  Rsrc2=Rsrc2+2
    //011  Rsrc2=Rsrc2+4
    //100  Rsrc2=Rsrc2
    //101  Rsrc2=Rsrc2-1
    //110  Rsrc2=Rsrc2-2
    //111  Rsrc2=Rsrc2-4
    //[14:10] src2
    //[9:5]src1
    //[4:0]dst
function Bit#(4) instr_JMPUNC; instr_JMPUNC= 4'd2;endfunction
//unconditional jump
    //[31:28]   jump
    //[27:26]   type of jumps
    //0 - jmp   pc[25:0]=code[25:0]
    //1 - jmp   pc=pc+offset(relaitive jump)    jmpr    R2(jmpr R2+4)
    //2 - call  pc=pc+offset, Rdst=pc         callr    R0,0xXXXXXXX,R1+4  or callr R0,R1+4
    //3 - ret   pc=Rdst                ret    Rx or ret Rx,Ry+-0,1,2,4
    //[25]
    //0 - offset = code[24:10](signed)
    //1 - offset = Rsrc2(signed)
    //[24:22]
    //000  Rsrc2=Rsrc2
    //001  Rsrc2=Rsrc2+1
    //010  Rsrc2=Rsrc2+2
    //011  Rsrc2=Rsrc2+4
    //100  Rsrc2=Rsrc2
    //101  Rsrc2=Rsrc2-1
    //110  Rsrc2=Rsrc2-2
    //111  Rsrc2=Rsrc2-4
    //[14:10] src2
    //[9:5]src1
    //[4:0]dst
function Bit#(4) instr_JMPF; instr_JMPF= 4'd3;endfunction
//conditional jumps
    //[31:28]   jump
    //[27:26]   type of jumps
    //0 - jmpz  pc=pc+offset
    //1 - jmpnz pc=pc+offset
    //2 - jmpc  pc=pc+offset
    //3 - jmpnc pc=pc+offset
    //[25]
    //0 - offset = code[24:10](signed)
    //1 - offset = Rsrc2(signed)
    //[24:22]
    //000  Rsrc2=Rsrc2
    //001  Rsrc2=Rsrc2+1
    //010  Rsrc2=Rsrc2+2
    //011  Rsrc2=Rsrc2+4
    //100  Rsrc2=Rsrc2
    //101  Rsrc2=Rsrc2-1
    //110  Rsrc2=Rsrc2-2
    //111  Rsrc2=Rsrc2-4
    //[14:10] src2
    //[9:5]src1
    //[4:0]dst

function Bit#(4) instr_ALU; instr_ALU= 4'd4;endfunction
// AND, OR, XOR, ADD, MUL, SHR, SHL
    //[31:28]ALU_op
    //[27:25] type of op
    //0- AND
    //1- OR
    //2- XOR
    //3- ADD
    //4- MUL
    //5-shift Rsrc1 left by Rscr2 ...0 MSB->C flag
    //6-shift Rsrc1 right by Rscr2 ...0 LSB ->C flag
    //7-CMP compare,
    //[24:22]
    //000  Rsrc2=Rsrc2
    //001  Rsrc2=Rsrc2+1
    //010  Rsrc2=Rsrc2+2
    //011  Rsrc2=Rsrc2+4
    //100  Rsrc2=Rsrc2
    //101  Rsrc2=Rsrc2-1
    //110  Rsrc2=Rsrc2-2
    //111  Rsrc2=Rsrc2-4
    //[14:10] src2
    //[9:5]src1
    //[4:0]dst
//} OPCODE;
function Bit#(4) instr_LDRF; instr_LDRF= 4'd5;endfunction
//LDRF Rdst,Rsrc1,-+Rsrc2
    //[31:28]LDRF_op
    //[27:26] type of LDRF op
    //0-LDRZ Rdst=Rsrc1 if z=1, otherwise Rdst=Rsrc2
    //1-LDRNZ Rdst=Rsrc1 if z=0, otherwise Rdst=Rsrc2
    //2-LDRC Rdst=Rsrc1 if c=1, otherwise Rdst=Rsrc2
    //3-LDRNC Rdst=Rsrc1 if c=0, otherwise Rdst=Rsrc2
    //[24:22]
    //000  Rsrc2=Rsrc2
    //001  Rsrc2=Rsrc2+1
    //010  Rsrc2=Rsrc2+2
    //011  Rsrc2=Rsrc2+4
    //100  Rsrc2=Rsrc2
    //101  Rsrc2=Rsrc2-1
    //110  Rsrc2=Rsrc2-2
    //111  Rsrc2=Rsrc2-4
    //[14:10] src2
    //[9:5]src1
    //[4:0]dst

function Bit#(6)  instr_NOP; instr_NOP = 6'd0; endfunction
//ldr
function Bit#(6)  instr_LDRR; instr_LDRR = {instr_LDR,2'b00}; endfunction
function Bit#(6)  instr_LDRP; instr_LDRP = {instr_LDR,2'b11};endfunction
function Bit#(11) instr_LDRH; instr_LDRH = {instr_LDR,7'b01_0_000_0};endfunction
function Bit#(11) instr_LDRL; instr_LDRL = {instr_LDR,7'b10_0_000_0};endfunction

//str
function Bit#(6)  instr_STRP;instr_STRP  = {instr_STR,2'b11}; endfunction
//alu
function Bit#(7)  instr_AND; instr_AND  = {instr_ALU,3'd0}; endfunction
function Bit#(7)  instr_OR; instr_OR    = {instr_ALU,3'd1}; endfunction
function Bit#(7)  instr_XOR; instr_XOR  = {instr_ALU,3'd2}; endfunction
function Bit#(7)  instr_ADD; instr_ADD  = {instr_ALU,3'd3}; endfunction
function Bit#(7)  instr_MUL; instr_MUL  = {instr_ALU,3'd4}; endfunction
function Bit#(7)  instr_SHL; instr_SHL  = {instr_ALU,3'd5}; endfunction
function Bit#(7)  instr_SHR; instr_SHR  = {instr_ALU,3'd6}; endfunction
function Bit#(7)  instr_CMP; instr_CMP  = {instr_ALU,3'd7}; endfunction

//jmp
function Bit#(6)  instr_JMP; instr_JMP   = {instr_JMPUNC,2'd0}; endfunction
function Bit#(6)  instr_JMPR; instr_JMPR = {instr_JMPUNC,2'd1}; endfunction
function Bit#(6)  instr_CALL; instr_CALL = {instr_JMPUNC,2'd2}; endfunction
function Bit#(6)  instr_RET; instr_RET   = {instr_JMPUNC,2'd3}; endfunction

function Bit#(6)  instr_JMPZ; instr_JMPZ   = {instr_JMPF,2'd0}; endfunction
function Bit#(6)  instr_JMPNZ; instr_JMPNZ = {instr_JMPF,2'd1}; endfunction
function Bit#(6)  instr_JMPC; instr_JMPC   = {instr_JMPF,2'd2}; endfunction
function Bit#(6)  instr_JMPNC; instr_JMPNC = {instr_JMPF,2'd3}; endfunction


function Bit#(7)  instr_LDRZ; instr_LDRZ   = {instr_LDRF,2'd0,1'b0}; endfunction
function Bit#(7)  instr_LDRNZ; instr_LDRNZ = {instr_LDRF,2'd1,1'b0}; endfunction
function Bit#(7)  instr_LDRC; instr_LDRC   = {instr_LDRF,2'd2,1'b0}; endfunction
function Bit#(7)  instr_LDRNC; instr_LDRNC = {instr_LDRF,2'd3,1'b0}; endfunction

//common
function Bool  instr_OFFSET_CODE; instr_OFFSET_CODE = False; endfunction
function Bool  instr_OFFSET_R; instr_OFFSET_R = True; endfunction

function Bit#(3)  instr_INCR_0; instr_INCR_0 = 3'b000; endfunction
function Bit#(3)  instr_DECR_0; instr_DECR_0 = 3'b000; endfunction
function Bit#(3)  instr_INCR_1; instr_INCR_1 = 3'b001; endfunction
function Bit#(3)  instr_INCR_2; instr_INCR_2 = 3'b010; endfunction
function Bit#(3)  instr_INCR_4; instr_INCR_4 = 3'b011; endfunction
function Bit#(3)  instr_DECR_1; instr_DECR_1 = 3'b101; endfunction
function Bit#(3)  instr_DECR_2; instr_DECR_2 = 3'b110; endfunction
function Bit#(3)  instr_DECR_4; instr_DECR_4 = 3'b111; endfunction
//
typedef enum
  { R0 , R1 , R2 , R3 , R4 , R5 , R6 , R7 ,
    R8 , R9 , R10, R11, R12, R13, R14, R15,
    R16, R17, R18, R19, R20, R21, R22, R23,
    R24, R25, R26, R27, R28, R29, R30, R31 } Rset deriving (Bits, Eq);
endpackage
