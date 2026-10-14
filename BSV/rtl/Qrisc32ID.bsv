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
        FIFOF#(Pipe_s) retiredExFifo,
        FIFOF#(Pipe_s) retiredMemFifo,
        RF_IFC rf,
        Bool flush_request
    ) (Empty);

    Reg#(Bit#(32)) pendingEx <- mkReg(0);
    Reg#(Bit#(32)) pendingMem <- mkReg(0);
    Reg#(Bit#(4)) pendingFlags <- mkReg(0);
    Reg#(Bool) pendingControl <- mkReg(False);

    function Bit#(32) regMask(RegIdx idx);
        return 32'h1 << idx;
    endfunction

    function Bool writesFlags(Pipe_s p);
        return p.and_op || p.or_op || p.xor_op || p.add_op || p.mul_op
            || p.shl_op || p.shr_op || p.cmp_op;
    endfunction

    function Bit#(32) readMask(Instruction inst_st, Pipe_s p);
        Bit#(32) instruction = inst_st.data;
        Opcode opcode = unpack(instruction[31:28]);
        LoadMode load_mode = unpack(instruction[27:26]);
        JumpMode jump_mode = unpack(instruction[27:26]);
        Bit#(32) mask = 0;
        Bool use_r1 = opcode == op_alu || opcode == op_ldrf || opcode == op_str
            || (opcode == op_ldr && (load_mode == LoadReg || load_mode == LoadMem));
        Bool use_r2 = opcode == op_alu || opcode == op_ldrf
            || (opcode == op_ldr && load_mode == LoadReg && p.incr_r2_enable)
            || (instruction[25] == 1 &&
                ((opcode == op_ldr && load_mode == LoadMem) || opcode == op_str
                 || (opcode == op_jmpunc && jump_mode != JumpAbs) || opcode == op_jmpf));
        Bool use_dst = opcode == op_str
            || (opcode == op_ldr && (load_mode == LoadHigh || load_mode == LoadLow))
            || (opcode == op_jmpunc && jump_mode == JumpRet);
        if (use_r1) mask = mask | regMask(p.src_r1);
        if (use_r2) mask = mask | regMask(p.src_r2);
        if (use_dst) mask = mask | regMask(p.dst_r);
        return instruction == 0 ? 0 : mask;
    endfunction

    function Pipe_s decode(Instruction inst_st);
        Bit#(32) instruction = inst_st.data;
        Word32 pc = inst_st.address + 8;

        Pipe_s p = defaultValue;

        p.dst_r  = instruction[4:0];
        p.src_r1 = instruction[9:5];
        p.src_r2 = instruction[14:10];
        p.incr_r2 = 0;
        p.incr_r2_enable = True;

        p.val_r1 = rf.read_r1(p.src_r1);
        p.val_r2 = rf.read_r2(p.src_r2);
        p.val_dst = rf.read_dst(p.dst_r);

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

    (* descending_urgency = "flush_instructionFifo, feed_instructionFifo" *)
    rule flush_instructionFifo(flush_request);
        instructionFifo.clear;
    endrule

    rule feed_instructionFifo(!flush_request);
        Bit#(32) nextEx = pendingEx;
        Bit#(32) nextMem = pendingMem;
        Bit#(4) nextFlags = pendingFlags;
        Bool nextControl = pendingControl;

        if (retiredExFifo.notEmpty) begin
            let p = retiredExFifo.first;
            retiredExFifo.deq;
            if (p.write_reg && !p.read_mem)
                nextEx = nextEx & ~regMask(p.dst_r);
            if (p.incr_r2_enable)
                nextEx = nextEx & ~regMask(p.src_r2);
            if (writesFlags(p))
                nextFlags = nextFlags - 1;
            if (p.jmpunc || ((p.jmpz || p.jmpnz || p.jmpc || p.jmpnc) && !p.ldrf_op))
                nextControl = False;
        end
        if (retiredMemFifo.notEmpty) begin
            let p = retiredMemFifo.first;
            retiredMemFifo.deq;
            if (p.write_reg)
                nextMem = nextMem & ~regMask(p.dst_r);
        end

        if (instructionFifo.notEmpty && decodeFifo.notFull) begin
            Instruction i = instructionFifo.first;
            Pipe_s p = decode(i);
            Bit#(32) writes = 0;
            if (p.write_reg) writes = writes | regMask(p.dst_r);
            if (p.incr_r2_enable) writes = writes | regMask(p.src_r2);
            Bool control = p.jmpunc || ((p.jmpz || p.jmpnz || p.jmpc || p.jmpnc) && !p.ldrf_op);
            Opcode opcode = unpack(i.data[31:28]);
            Bool flagHazard = (opcode == op_jmpf || opcode == op_ldrf) && pendingFlags != 0;
            if (!pendingControl && !flagHazard
                && ((readMask(i, p) | writes) & (pendingEx | pendingMem)) == 0) begin
                instructionFifo.deq;
                decodeFifo.enq(p);
                if (p.write_reg) begin
                    if (p.read_mem) nextMem = nextMem | regMask(p.dst_r);
                    else nextEx = nextEx | regMask(p.dst_r);
                end
                if (p.incr_r2_enable)
                    nextEx = nextEx | regMask(p.src_r2);
                if (writesFlags(p))
                    nextFlags = nextFlags + 1;
                if (control) nextControl = True;
            end
        end

        pendingEx <= nextEx;
        pendingMem <= nextMem;
        pendingFlags <= nextFlags;
        pendingControl <= nextControl;
    endrule
endmodule
