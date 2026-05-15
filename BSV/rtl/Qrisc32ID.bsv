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

module qrisc32_ID#(
        FIFOF#(Instruction) instructionFifo,
        FIFOF#(Pipe_s) decodeFifo,
        RF_IFC rf,
        WB_IFC writebackStage,
        Bool flush_request
    ) (Empty);

    Reg#(Bit#(4)) bubbleCounter <- mkReg(0);
    Reg#(Bit#(4)) flushDrainCounter <- mkReg(0);

    function Pipe_s apply_forwarding(Pipe_s p, Pipe_s wb_mem_fwd, Pipe_s wb_ex_fwd);
        p.val_r1 =   (wb_mem_fwd.write_reg && wb_mem_fwd.dst_r == p.src_r1) ? wb_mem_fwd.val_dst :
                    ((wb_ex_fwd.write_reg && wb_ex_fwd.dst_r == p.src_r1) ? wb_ex_fwd.val_dst :
                     (wb_ex_fwd.incr_r2_enable && wb_ex_fwd.src_r2 == p.src_r1) ? wb_ex_fwd.val_r2 :
                      rf.read_r1(p.src_r1));

        p.val_r2 =   (wb_mem_fwd.write_reg && wb_mem_fwd.dst_r == p.src_r2) ? wb_mem_fwd.val_dst :
                    ((wb_ex_fwd.write_reg && wb_ex_fwd.dst_r == p.src_r2) ? wb_ex_fwd.val_dst :
                     (wb_ex_fwd.incr_r2_enable && wb_ex_fwd.src_r2 == p.src_r2) ? wb_ex_fwd.val_r2 :
                      rf.read_r2(p.src_r2));

        p.val_dst =  (wb_mem_fwd.write_reg && wb_mem_fwd.dst_r == p.dst_r) ? wb_mem_fwd.val_dst :
                    ((wb_ex_fwd.write_reg && wb_ex_fwd.dst_r == p.dst_r) ? wb_ex_fwd.val_dst :
                     (wb_ex_fwd.incr_r2_enable && wb_ex_fwd.src_r2 == p.dst_r) ? wb_ex_fwd.val_r2 :
                      rf.read_dst(p.dst_r));
        return p;
    endfunction

    function Pipe_s decode(Instruction inst_st, Pipe_s wb_mem_fwd, Pipe_s wb_ex_fwd);
        Bit#(32) instruction = inst_st.data;
        Word32 pc = inst_st.address + 8;

        Pipe_s p = defaultValue;

        p.dst_r  = instruction[4:0];
        p.src_r1 = instruction[9:5];
        p.src_r2 = instruction[14:10];
        p.incr_r2 = 0;
        p.incr_r2_enable = True;

        p = apply_forwarding(p, wb_mem_fwd, wb_ex_fwd);

        Word32 offset_w = (instruction[25] == 1) ? p.val_r2 : sext15(instruction[24:10]);
        Opcode opcode = unpack(instruction[31:28]);
        LoadMode load_mode = unpack(instruction[27:26]);
        JumpMode jump_mode = unpack(instruction[27:26]);
        FlagMode flag_mode = unpack(instruction[27:26]);
        AluOp alu_op = unpack(instruction[27:25]);
        IncrMode incr_mode = unpack(instruction[24:22]);

        case (incr_mode)
            IncrNone:    begin p.incr_r2 = 0;  p.incr_r2_enable = False; end
            IncrOne:     begin p.incr_r2 = 1;  p.incr_r2_enable = True;  end
            IncrTwo:     begin p.incr_r2 = 2;  p.incr_r2_enable = True;  end
            IncrFour:    begin p.incr_r2 = 4;  p.incr_r2_enable = True;  end
            IncrNoneAlt: begin p.incr_r2 = 0;  p.incr_r2_enable = False; end
            DecrOne:     begin p.incr_r2 = -1; p.incr_r2_enable = True;  end
            DecrTwo:     begin p.incr_r2 = -2; p.incr_r2_enable = True;  end
            DecrFour:    begin p.incr_r2 = -4; p.incr_r2_enable = True;  end
        endcase

        case (opcode)
            op_ldr: begin
                case (load_mode)
                    LoadReg: begin
                        p.write_reg = (p.dst_r != p.src_r1);
                        p.val_dst = p.val_r1;
                    end
                    LoadHigh: begin
                        p.val_dst[31:16] = instruction[20:5];
                        p.write_reg = True;
                        p.incr_r2_enable = False;
                    end
                    LoadLow: begin
                        p.val_dst[15:0] = instruction[20:5];
                        p.write_reg = True;
                        p.incr_r2_enable = False;
                    end
                    default: begin
                        p.read_mem = True;
                        p.write_reg = True;
                        p.incr_r2_enable = (instruction[25] == 1);
                        p.val_r2 = offset_w;
                    end
                endcase
            end

            op_str: begin
                p.write_mem = True;
                p.val_r2 = offset_w;
                p.incr_r2_enable = (instruction[25] == 1);
            end

            op_jmpunc: begin
                case (jump_mode)
                    JumpAbs: begin
                        p.val_r1 = zext26(instruction[25:0]);
                        p.val_r2 = 0;
                        p.incr_r2_enable = False;
                        p.jmpunc = True;
                    end
                    JumpRel: begin
                        p.val_r1 = pc;
                        p.val_r2 = offset_w;
                        p.incr_r2_enable = (instruction[25] == 1);
                        p.jmpunc = True;
                    end
                    JumpCall: begin
                        p.val_r1 = pc;
                        p.val_r2 = offset_w;
                        p.val_dst = pc;
                        p.incr_r2_enable = (instruction[25] == 1);
                        p.jmpunc = True;
                        p.write_reg = True;
                    end
                    JumpRet: begin
                        p.val_r1 = p.val_dst;
                        p.val_r2 = 0;
                        p.incr_r2_enable = (instruction[25] == 1);
                        p.jmpunc = True;
                    end
                endcase
            end

            op_jmpf: begin
                case (flag_mode)
                    FlagZ:  begin p.jmpz  = True; end
                    FlagNZ: begin p.jmpnz = True; end
                    FlagC:  begin p.jmpc  = True; end
                    FlagNC: begin p.jmpnc = True; end
                endcase
                p.val_r1 = pc;
                p.val_r2 = offset_w;
                p.incr_r2_enable = (instruction[25] == 1);
            end

            op_alu: begin
                case (alu_op)
                    AluAnd: begin p.write_reg = True; p.and_op = True; end
                    AluOr:  begin p.write_reg = True; p.or_op  = True; end
                    AluXor: begin p.write_reg = True; p.xor_op = True; end
                    AluAdd: begin p.write_reg = True; p.add_op = True; end
                    AluMul: begin p.write_reg = True; p.mul_op = True; end
                    AluShl: begin p.write_reg = True; p.shl_op = True; end
                    AluShr: begin p.write_reg = True; p.shr_op = True; end
                    AluCmp: begin p.cmp_op = True; end
                endcase
            end

            op_ldrf: begin
                case (flag_mode)
                    FlagZ:  begin p.jmpz  = True; end
                    FlagNZ: begin p.jmpnz = True; end
                    FlagC:  begin p.jmpc  = True; end
                    FlagNC: begin p.jmpnc = True; end
                endcase
                p.ldrf_op = True;
                p.write_reg = True;
            end

            default: p = defaultValue;
        endcase

        return p;
    endfunction

    (* descending_urgency = "flush_instructionFifo, insert_hazard_bubble, feed_instructionFifo" *)
    rule flush_instructionFifo(flush_request);
        if (instructionFifo.notEmpty)
            instructionFifo.deq;
        bubbleCounter <= 6;
        flushDrainCounter <= 1;
    endrule

    rule insert_hazard_bubble(
        !flush_request && bubbleCounter != 0 && decodeFifo.notFull
    );
        decodeFifo.enq(defaultValue);
        bubbleCounter <= bubbleCounter - 1;
        if (flushDrainCounter != 0) begin
            if (instructionFifo.notEmpty) begin
                instructionFifo.deq;
                flushDrainCounter <= flushDrainCounter - 1;
            end
            else begin
                flushDrainCounter <= 0;
            end
        end
    endrule

    rule feed_instructionFifo(
        instructionFifo.notEmpty && decodeFifo.notFull && !flush_request && bubbleCounter == 0
    );
        Instruction i = instructionFifo.first;
        instructionFifo.deq;

        Pipe_s wb_ex_fwd = writebackStage.forward_ex();
        Pipe_s wb_mem_fwd = writebackStage.forward_mem();
        decodeFifo.enq(decode(i, wb_mem_fwd, wb_ex_fwd));
        if (i.data != 0)
            bubbleCounter <= 6;
        flushDrainCounter <= 0;
    endrule
endmodule
