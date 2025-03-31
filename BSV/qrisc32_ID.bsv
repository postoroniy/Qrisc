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
//////////////////////////////////////////////////////////////////////////////////////////////
import Qrisc_pack::*;
import FIFOF::*;
import Vector::*;

module qrisc32_ID#(
        FIFOF#(Instruction) instructionFifo,
        FIFOF#(Pipe_s) decodeFifo,
        FIFOF#(Pipe_s) wbExFifo,
        FIFOF#(Pipe_s) wbMemFifo
    ) (Empty);

    Vector#(32, Reg#(Bit#(32))) rf <- replicateM(mkRegA(0));

    function Pipe_s decode(Instruction instruction_st, Pipe_s wb_mem, Pipe_s wb_ex);
        InstructionCode instruction = unpack(instruction_st.data);
        let pc = instruction_st.address;

        Pipe_s pipe_id_out = defaultValue;

        pipe_id_out.dst_r = R0;
        pipe_id_out.src_r1 = R0;
        pipe_id_out.src_r2 = R0;
        pipe_id_out.incr2 = NoChanges;

        pipe_id_out.read_mem = False;
        pipe_id_out.write_mem = False;
        pipe_id_out.write_reg = False;

        pipe_id_out.mul_op = False;
        pipe_id_out.add_op = False;
        pipe_id_out.or_op = False;
        pipe_id_out.and_op = False;
        pipe_id_out.xor_op = False;
        pipe_id_out.shl_op = False;
        pipe_id_out.shr_op = False;
        pipe_id_out.cmp_op = False;

        pipe_id_out.pc_change_op = False;
        pipe_id_out.check_z = False;
        pipe_id_out.check_nz = False;
        pipe_id_out.check_c = False;
        pipe_id_out.check_nc = False;

        case (instruction) matches
            tagged MOVF {.opcode}: begin
                let flags = opcode.flags;
                case (flags) matches
                    tagged Zero:        pipe_id_out.check_z = True;
                    tagged NotZero:     pipe_id_out.check_nz = True;
                    tagged Carry:       pipe_id_out.check_c = True;
                    tagged NotCarry:    pipe_id_out.check_nc = True;
                endcase
                let common = opcode.common;
                pipe_id_out.src_r2      = common.src2;
                pipe_id_out.src_r1      = common.src1;
                pipe_id_out.dst_r       = common.dst;
                pipe_id_out.incr2       = common.incr2;
                pipe_id_out.write_reg   = True;
            end

            tagged LDRHF {.opcode}: begin
                let flags = opcode.flags;
                case (flags) matches
                    tagged Zero:        pipe_id_out.check_z = True;
                    tagged NotZero:     pipe_id_out.check_nz = True;
                    tagged Carry:       pipe_id_out.check_c = True;
                    tagged NotCarry:    pipe_id_out.check_nc = True;
                endcase
                let immCode = opcode.immCode;
                pipe_id_out.dst_r           = immCode.dst;
                pipe_id_out.val_dst         = rf[pack(immCode.dst)];
                pipe_id_out.val_dst[31:16]  = immCode.imData;
                pipe_id_out.write_reg       = True;
            end

            tagged LDRLF {.opcode}: begin
                let flags = opcode.flags;
                case (flags) matches
                    tagged Zero:        pipe_id_out.check_z = True;
                    tagged NotZero:     pipe_id_out.check_nz = True;
                    tagged Carry:       pipe_id_out.check_c = True;
                    tagged NotCarry:    pipe_id_out.check_nc = True;
                endcase
                let immCode = opcode.immCode;
                pipe_id_out.dst_r           = immCode.dst;
                pipe_id_out.val_dst         = rf[pack(immCode.dst)];
                pipe_id_out.val_dst[15:0]   = immCode.imData;
                pipe_id_out.write_reg       = True;
            end

            tagged LDR_Offset {.opcode}: begin
                // Rdst=[Rsrc1+offset]
                let offset = opcode.offset;
                case(offset) matches
                    tagged OffsetCode {.offsetCode}: begin
                        //offset from instruction code
                        pipe_id_out.val_r1   = rf[pack(offsetCode.src1)];
                        pipe_id_out.val_r2   = zeroExtend(offsetCode.imData);
                        pipe_id_out.dst_r    = offsetCode.dst;
                        pipe_id_out.read_mem = True;
                    end
                    tagged CommonFields {.commonFields}: begin
                        // offset = src2
                        pipe_id_out.val_r1      = rf[pack(commonFields.src1)];
                        pipe_id_out.val_r2      = rf[pack(commonFields.src2)];
                        pipe_id_out.incr2       = commonFields.incr2;
                        pipe_id_out.dst_r       = commonFields.dst;
                        pipe_id_out.read_mem    = True;
                    end
                endcase
            end

            tagged STR_Offset {.opcode}: begin
                //[Rsrc1+offset]=Rdst
                let offset = opcode.offset;
                case(offset) matches
                    tagged OffsetCode {.offsetCode}: begin
                        //offset from instruction code
                        pipe_id_out.val_r1   = rf[pack(offsetCode.src1)];
                        pipe_id_out.val_r2   = zeroExtend(offsetCode.imData);
                        pipe_id_out.val_dst  = rf[pack(offsetCode.dst)];
                        pipe_id_out.write_mem = True;
                    end
                    tagged CommonFields {.commonFields}: begin
                        // offset = src2
                        pipe_id_out.val_r1      = rf[pack(commonFields.src1)];
                        pipe_id_out.val_r2      = rf[pack(commonFields.src2)];
                        pipe_id_out.incr2       = commonFields.incr2;
                        pipe_id_out.val_dst     = rf[pack(commonFields.dst)];
                        pipe_id_out.write_mem   = True;
                    end
                endcase
            end

            tagged JMPF {.opcode}: begin
                // jmp   pc[25:0]=code[25:0] , cond or uncond depending on flags
                let flags = opcode.flags;
                case (flags) matches
                    tagged Zero:        pipe_id_out.check_z = True;
                    tagged NotZero:     pipe_id_out.check_nz = True;
                    tagged Carry:       pipe_id_out.check_c = True;
                    tagged NotCarry:    pipe_id_out.check_nc = True;
                endcase
                pipe_id_out.val_r1       = zeroExtend(opcode.imData);
                pipe_id_out.pc_change_op = True;
            end

            tagged JMPRF {.opcode}: begin
                //pc=pc+offset(relaitive jump)    jmpr    R2(jmpr R2+4)
                let flags = opcode.flags;
                case (flags) matches
                    tagged Zero:        pipe_id_out.check_z = True;
                    tagged NotZero:     pipe_id_out.check_nz = True;
                    tagged Carry:       pipe_id_out.check_c = True;
                    tagged NotCarry:    pipe_id_out.check_nc = True;
                endcase
                let offset = opcode.offset;
                case(offset) matches
                    tagged OffsetCode {.offsetCode}: begin
                        //offset from instruction code
                        pipe_id_out.val_r1   = rf[pack(offsetCode.src1)];
                        pipe_id_out.val_r2   = zeroExtend(offsetCode.imData);
                        pipe_id_out.pc_change_op = True;
                    end
                    tagged CommonFields {.commonFields}: begin
                        // offset = src2
                        pipe_id_out.val_r1      = rf[pack(commonFields.src1)];
                        pipe_id_out.val_r2      = rf[pack(commonFields.src2)];
                        pipe_id_out.incr2       = commonFields.incr2;
                        pipe_id_out.pc_change_op = True;
                    end
                endcase
            end
            tagged CALLRF {.opcode}: begin
                //pc=pc+offset, Rdst=pc         callr    R0,0xXXXXXXX,R1+4  or callr R0,R1+4
                let flags = opcode.flags;
                case (flags) matches
                    tagged Zero:        pipe_id_out.check_z = True;
                    tagged NotZero:     pipe_id_out.check_nz = True;
                    tagged Carry:       pipe_id_out.check_c = True;
                    tagged NotCarry:    pipe_id_out.check_nc = True;
                endcase
                let offset = opcode.offset;
                case(offset) matches
                    tagged OffsetCode {.offsetCode}: begin
                        //offset from instruction code
                        pipe_id_out.val_r1   = rf[pack(offsetCode.src1)];
                        pipe_id_out.val_r2   = zeroExtend(offsetCode.imData);
                        pipe_id_out.dst_r    = offsetCode.dst;
                        pipe_id_out.pc_change_op = True;
                        pipe_id_out.write_reg = True;
                    end
                    tagged CommonFields {.commonFields}: begin
                        // offset = src2
                        pipe_id_out.val_r1      = rf[pack(commonFields.src1)];
                        pipe_id_out.val_r2      = rf[pack(commonFields.src2)];
                        pipe_id_out.incr2       = commonFields.incr2;
                        pipe_id_out.dst_r       = commonFields.dst;
                        pipe_id_out.val_dst     = pc;

                        pipe_id_out.pc_change_op = True;
                        pipe_id_out.write_reg   = True;
                    end
                endcase
            end
            tagged RETF {.opcode}: begin
                //pc=Rdst                ret    Rx or ret Rx,Ry+-0,1,2,4
                let flags = opcode.flags;
                case (flags) matches
                    tagged Zero:        pipe_id_out.check_z = True;
                    tagged NotZero:     pipe_id_out.check_nz = True;
                    tagged Carry:       pipe_id_out.check_c = True;
                    tagged NotCarry:    pipe_id_out.check_nc = True;
                endcase
                let common = opcode.common;
                pipe_id_out.val_r1      = rf[pack(common.src1)];
                pipe_id_out.val_r2      = rf[pack(common.src2)];
                pipe_id_out.incr2       = common.incr2;
                pipe_id_out.dst_r       = common.dst;
                pipe_id_out.val_dst     = rf[pack(common.dst)];

                pipe_id_out.pc_change_op = True;
                pipe_id_out.write_reg   = True;
            end
            tagged ALU {.opcode}: begin
                let common = opcode.common;
                let alutype = opcode.alutype;
                case (alutype)
                    AND: pipe_id_out.and_op = True;
                    OR: pipe_id_out.or_op = True;
                    XOR: pipe_id_out.xor_op = True;
                    ADD: pipe_id_out.add_op = True;
                    MUL: pipe_id_out.mul_op = True;
                    SHL: pipe_id_out.shl_op = True;
                    SHR: pipe_id_out.shr_op = True;
                    CMP: pipe_id_out.cmp_op = True;
                endcase
                pipe_id_out.src_r2      = common.src2;
                pipe_id_out.src_r1      = common.src1;
                pipe_id_out.dst_r       = common.dst;
                pipe_id_out.incr2       = common.incr2;
                pipe_id_out.val_r1      = rf[pack(common.src1)];
                pipe_id_out.val_r2      = rf[pack(common.src2)];
                pipe_id_out.val_dst     = rf[pack(common.dst)];
                pipe_id_out.write_reg   = True;
            end
            default: pipe_id_out = defaultValue;
        endcase

        pipe_id_out.val_r1  =   (wb_mem.write_reg && wb_mem.dst_r == pipe_id_out.src_r1) ?  wb_mem.val_dst :
                                (  wb_ex.write_reg && wb_ex.dst_r == pipe_id_out.src_r1) ?  wb_ex.val_dst :
                                (wb_ex.incr2!=NoChanges && wb_ex.src_r2 == pipe_id_out.src_r1) ? wb_ex.val_r2 :
                                rf[pack(pipe_id_out.src_r1)];

        pipe_id_out.val_r2 =    (wb_mem.write_reg && wb_mem.dst_r == pipe_id_out.src_r2) ? wb_mem.val_dst :
                                (  wb_ex.write_reg && wb_ex.dst_r == pipe_id_out.src_r2) ? wb_ex.val_dst :
                                (wb_ex.incr2!=NoChanges && wb_ex.src_r2 == pipe_id_out.src_r2) ? wb_ex.val_r2 :
                                rf[pack(pipe_id_out.src_r2)];

        pipe_id_out.val_dst =   (wb_mem.write_reg && wb_mem.dst_r == pipe_id_out.dst_r) ? wb_mem.val_dst :
                                (  wb_ex.write_reg && wb_ex.dst_r == pipe_id_out.dst_r) ? wb_ex.val_dst :
                                (wb_ex.incr2!=NoChanges && wb_ex.src_r2 == pipe_id_out.dst_r) ? wb_ex.val_r2 :
                                rf[pack(pipe_id_out.dst_r)];
        return pipe_id_out;
    endfunction

    rule feed_instructionFifo(instructionFifo.notEmpty && decodeFifo.notFull);
        Instruction i = instructionFifo.first;
        instructionFifo.deq;

        Pipe_s wb_ex = defaultValue;
        wb_ex = wbExFifo.first;
        Pipe_s wb_mem = defaultValue;
        wb_mem = wbMemFifo.first;

        if (wbMemFifo.notEmpty) begin
            wbMemFifo.deq();
            if (wb_mem.write_reg) // from memory read stage
                rf[pack(wb_mem.dst_r)] <= wb_mem.val_dst;
        end
        if (wbExFifo.notEmpty) begin
            wbExFifo.deq();

            // from ex stage R2 register
            if( wb_ex.incr2 != NoChanges && wb_ex.src_r2 != wb_mem.dst_r)
                rf[pack(wb_ex.src_r2)] <= wb_ex.val_r2;

            // from ex stage DST register
            if( wb_ex.incr2 != NoChanges && wb_ex.src_r2 != wb_ex.dst_r &&
                wb_ex.write_reg && wb_ex.dst_r != wb_mem.dst_r)
                rf[pack(wb_ex.dst_r)] <= wb_ex.val_dst;
        end

        let pipe_id_out = decode(i, wb_mem, wb_ex);
        decodeFifo.enq(pipe_id_out);
    endrule
endmodule
