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

package Qrisc_pack;
    typedef struct packed{
        logic[31:0]       val_r1;//value of register src1
        logic[31:0]       val_r2;//value of register src2
        logic[31:0]       val_dst;//value of register dst

        logic[4:0]        src_r2;//indicate number of src2 register
        logic[4:0]        src_r1;//indicate number of src1 register
        logic[4:0]        dst_r;//indicate number of dest register

        //add to src2
        logic[3:0]        incr_r2;//0 +1 or -1, +2,-2, +4, -4
        logic             incr_r2_enable;//

        //
        logic             write_reg;//indicate write to RF(addres in dst_r, value in dst_v)
        //load store operations, if both bit is zero then bypass MEM stage
        logic             read_mem;//indicate read from memory(addres in src1+src2)
        logic             write_mem;//indicate write to memory(addres in src1+src2, value in dst)
        //

        //if alu and shift operations are  zeros then bypass EX stage
        //alu operations
        logic             and_op;//AND
        logic             or_op;// OR
        logic             xor_op;//XOR
        logic             add_op;//+
        logic             mul_op;//
        logic             cmp_op;//compare operation

        logic             ldrf_op;//conditional load
        //shifter operations
        logic             shl_op;//shift left
        logic             shr_op;//shift  right
        //jmp operations
        //old pc in  value in    val_r1
        //offset to  pc  value in    val_r2
        //new pc value in    val_dst
        //types of jump
        //indicate to calc new address of PC
        logic             jmpunc;
        logic             jmpz;
        logic             jmpnz;
        logic             jmpc;
        logic             jmpnc;
    } pipe_struct_t;

//operations
typedef enum logic [3:0] {
    OP_LDR    = 4'd0,
    OP_STR    = 4'd1,
    OP_JMPUNC = 4'd2,
    OP_JMPF   = 4'd3,
    OP_ALU    = 4'd4,
    OP_LDRF   = 4'd5
} opcode_t;

typedef enum logic [1:0] {
    LOAD_REG  = 2'd0,
    LOAD_HIGH = 2'd1,
    LOAD_LOW  = 2'd2,
    LOAD_MEM  = 2'd3
} load_mode_t;

typedef enum logic [1:0] {
    JUMP_ABS  = 2'd0,
    JUMP_REL  = 2'd1,
    JUMP_CALL = 2'd2,
    JUMP_RET  = 2'd3
} jump_mode_t;

typedef enum logic [1:0] {
    FLAG_Z  = 2'd0,
    FLAG_NZ = 2'd1,
    FLAG_C  = 2'd2,
    FLAG_NC = 2'd3
} flag_mode_t;

typedef enum logic [2:0] {
    ALU_AND = 3'd0,
    ALU_OR  = 3'd1,
    ALU_XOR = 3'd2,
    ALU_ADD = 3'd3,
    ALU_MUL = 3'd4,
    ALU_SHL = 3'd5,
    ALU_SHR = 3'd6,
    ALU_CMP = 3'd7
} alu_op_t;

typedef enum logic [0:0] {
    OFFSET_IMM = 1'b0,
    OFFSET_REG = 1'b1
} offset_mode_t;

typedef enum logic [2:0] {
    INCR_NONE     = 3'd0,
    INCR_ONE      = 3'd1,
    INCR_TWO      = 3'd2,
    INCR_FOUR     = 3'd3,
    INCR_NONE_ALT = 3'd4,
    DECR_ONE      = 3'd5,
    DECR_TWO      = 3'd6,
    DECR_FOUR     = 3'd7
} incr_mode_t;

localparam opcode_t
    LDR=OP_LDR;//LDR Rdst,[Rsrc1],-+Rsrc2
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
localparam opcode_t
    STR=OP_STR;//STR Rdst,[Rsrc1],-+Rsrc2
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
localparam opcode_t
    JMPUNC=OP_JMPUNC;//unconditional jump
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
localparam opcode_t
    JMPF=OP_JMPF;//conditional jumps
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

localparam opcode_t
    ALU=OP_ALU;// AND, OR, XOR,
    // ADD,  MUL,
    // SHR, SHL
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
localparam opcode_t
    LDRF=OP_LDRF;//LDRF Rdst,Rsrc1,-+Rsrc2
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


localparam logic[4+1:0]    NOP     ='0;
//ldr
localparam logic[4+1:0]    LDRR    ={LDR,LOAD_REG};
localparam logic[4+6:0]    LDRH    ={LDR,LOAD_HIGH,OFFSET_IMM,INCR_NONE,1'b0};
localparam logic[4+6:0]    LDRL    ={LDR,LOAD_LOW,OFFSET_IMM,INCR_NONE,1'b0};
localparam logic[4+1:0]    LDRP    ={LDR,LOAD_MEM};

//str
localparam logic[4+1:0]    STRP    ={STR,LOAD_MEM};

//alu
localparam logic[4+2:0]    AND     ={ALU,ALU_AND};
localparam logic[4+2:0]    OR      ={ALU,ALU_OR};
localparam logic[4+2:0]    XOR     ={ALU,ALU_XOR};
localparam logic[4+2:0]    ADD     ={ALU,ALU_ADD};
localparam logic[4+2:0]    MUL     ={ALU,ALU_MUL};
localparam logic[4+2:0]    SHL     ={ALU,ALU_SHL};
localparam logic[4+2:0]    SHR     ={ALU,ALU_SHR};
localparam logic[4+2:0]    CMP     ={ALU,ALU_CMP};

//jmp
localparam logic[4+1:0]    JMP     ={JMPUNC,JUMP_ABS};
localparam logic[4+1:0]    JMPR    ={JMPUNC,JUMP_REL};
localparam logic[4+1:0]    CALL    ={JMPUNC,JUMP_CALL};
localparam logic[4+1:0]    RET     ={JMPUNC,JUMP_RET};

localparam logic[4+1:0]    JMPZ    ={JMPF,FLAG_Z};
localparam logic[4+1:0]    JMPNZ   ={JMPF,FLAG_NZ};
localparam logic[4+1:0]    JMPC    ={JMPF,FLAG_C};
localparam logic[4+1:0]    JMPNC   ={JMPF,FLAG_NC};


localparam logic[4+2:0]    LDRZ    ={LDRF,FLAG_Z,1'b0};
localparam logic[4+2:0]    LDRNZ   ={LDRF,FLAG_NZ,1'b0};
localparam logic[4+2:0]    LDRC    ={LDRF,FLAG_C,1'b0};
localparam logic[4+2:0]    LDRNC   ={LDRF,FLAG_NC,1'b0};

//common
localparam offset_mode_t  OFFSET_CODE  = OFFSET_IMM;
localparam offset_mode_t  OFFSET_R     = OFFSET_REG;

localparam incr_mode_t    INCR_0=INCR_NONE;
localparam incr_mode_t    DECR_0=INCR_NONE;
localparam incr_mode_t    INCR_1=INCR_ONE;
localparam incr_mode_t    INCR_2=INCR_TWO;
localparam incr_mode_t    INCR_4=INCR_FOUR;
localparam incr_mode_t    DECR_1=DECR_ONE;
localparam incr_mode_t    DECR_2=DECR_TWO;
localparam incr_mode_t    DECR_4=DECR_FOUR;

localparam logic[4:0]      R0    =5'd0;
localparam logic[4:0]      R1    =5'd1;
localparam logic[4:0]      R2    =5'd2;
localparam logic[4:0]      R3    =5'd3;
localparam logic[4:0]      R4    =5'd4;
localparam logic[4:0]      R5    =5'd5;
localparam logic[4:0]      R6    =5'd6;
localparam logic[4:0]      R7    =5'd7;
localparam logic[4:0]      R8    =5'd8;
localparam logic[4:0]      R9    =5'd9;
localparam logic[4:0]      R10   =5'd10;
localparam logic[4:0]      R11   =5'd11;
localparam logic[4:0]      R12   =5'd12;
localparam logic[4:0]      R13   =5'd13;
localparam logic[4:0]      R14   =5'd14;
localparam logic[4:0]      R15   =5'd15;
localparam logic[4:0]      R16   =5'd16;
localparam logic[4:0]      R17   =5'd17;
localparam logic[4:0]      R18   =5'd18;
localparam logic[4:0]      R19   =5'd19;
localparam logic[4:0]      R20   =5'd20;
localparam logic[4:0]      R21   =5'd21;
localparam logic[4:0]      R22   =5'd22;
localparam logic[4:0]      R23   =5'd23;
localparam logic[4:0]      R24   =5'd24;
localparam logic[4:0]      R25   =5'd25;
localparam logic[4:0]      R26   =5'd26;
localparam logic[4:0]      R27   =5'd27;
localparam logic[4:0]      R28   =5'd28;
localparam logic[4:0]      R29   =5'd29;
localparam logic[4:0]      R30   =5'd30;
localparam logic[4:0]      R31   =5'd31;
endpackage
