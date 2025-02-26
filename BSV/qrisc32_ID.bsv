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
        FIFOF#(Pipe_s) decodeFifo
    ) (ID_ifc);

  Vector#(32, Reg#(Int#(32))) rf <- replicateM(mkRegA(0));

  Wire#(Pipe_s) pipe_wb_mem <- mkWire();
  Wire#(Pipe_s) pipe_wb_ex <-  mkWire();

  function Pipe_s decode(Instruction instruction_st, Pipe_s wb_mem, Pipe_s wb_ex);
    Pipe_s pipe_id_out = defaultValue;
    Int#(32) offset_w;

    let instruction = instruction_st.data;
    let pc = instruction_st.address;
    Bool incr_r2_enable = instruction[25]==1;

    pipe_id_out.dst_r = instruction[4:0];
    pipe_id_out.src_r1 = instruction[9:5];
    pipe_id_out.src_r2 = instruction[14:10];
    pipe_id_out.incr_r2 = 0;
    pipe_id_out.incr_r2_enable = True;

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
    pipe_id_out.ldrf_op = False;

    pipe_id_out.jmpunc = False;
    pipe_id_out.jmpz = False;
    pipe_id_out.jmpnz = False;
    pipe_id_out.jmpc = False;
    pipe_id_out.jmpnc = False;

    pipe_id_out.val_r1 = (wb_mem.write_reg && wb_mem.dst_r == pipe_id_out.src_r1) ? wb_mem.val_dst :
                         (wb_ex.write_reg && wb_ex.dst_r == pipe_id_out.src_r1) ? wb_ex.val_dst :
                         (wb_ex.incr_r2_enable && wb_ex.src_r2 == pipe_id_out.src_r1) ? wb_ex.val_r2 :
                         rf[pipe_id_out.src_r1];

    pipe_id_out.val_r2 = (wb_mem.write_reg && wb_mem.dst_r == pipe_id_out.src_r2) ? wb_mem.val_dst :
                         (wb_ex.write_reg && wb_ex.dst_r == pipe_id_out.src_r2) ? wb_ex.val_dst :
                         (wb_ex.incr_r2_enable && wb_ex.src_r2 == pipe_id_out.src_r2) ? wb_ex.val_r2 :
                         rf[pipe_id_out.src_r2];

    pipe_id_out.val_dst = (wb_mem.write_reg && wb_mem.dst_r == pipe_id_out.dst_r) ? wb_mem.val_dst :
                          (wb_ex.write_reg && wb_ex.dst_r == pipe_id_out.dst_r) ? wb_ex.val_dst :
                          (wb_ex.incr_r2_enable && wb_ex.src_r2 == pipe_id_out.dst_r) ? wb_ex.val_r2 :
                          rf[pipe_id_out.dst_r];

    offset_w = (instruction[25]==1) ? pipe_id_out.val_r2 :  signExtend(unpack(instruction[24:10]));

    case (unpack(instruction[24:22]))
      NoChanges0, NoChanges1: begin pipe_id_out.incr_r2 = 0; pipe_id_out.incr_r2_enable = False; end
      Incr1: pipe_id_out.incr_r2 = 1;
      Incr2: pipe_id_out.incr_r2 = 2;
      Incr4: pipe_id_out.incr_r2 = 4;
      Decr1: pipe_id_out.incr_r2 = -1;
      Decr2: pipe_id_out.incr_r2 = -2;
      Decr4: pipe_id_out.incr_r2 = -4;
    endcase

    case (unpack(instruction[31:28]))
      LDR: case (unpack(instruction[27:26]))
        Mov: begin
          pipe_id_out.write_reg = (pipe_id_out.dst_r != pipe_id_out.src_r1);// ? 1 : 0;
          pipe_id_out.val_dst = pipe_id_out.val_r1;
        end
        Ldrh: begin
          Bit#(16) val = unpack(instruction[20:5]);
          Bit#(32) val_dst = pack(pipe_id_out.val_dst);
          pipe_id_out.val_dst = unpack({val, val_dst[15:0]});
          pipe_id_out.write_reg = True;
          pipe_id_out.incr_r2_enable = False;
        end
        Ldrl: begin
          Bit#(16) val = unpack(instruction[20:5]);
          Bit#(32) val_dst = pack(pipe_id_out.val_dst);
          pipe_id_out.val_dst = unpack({val_dst[31:16],val});
          pipe_id_out.write_reg = True;
          pipe_id_out.incr_r2_enable = False;
        end
        Ldr_Offset: begin
          pipe_id_out.read_mem = True;
          pipe_id_out.write_reg = True;
          pipe_id_out.incr_r2_enable = incr_r2_enable;
          pipe_id_out.val_r2 = offset_w;
        end
      endcase

      STR: begin
       //case (instruction[27:26])
        // default: begin
          pipe_id_out.write_mem = True;
          pipe_id_out.val_r2 = offset_w;
          pipe_id_out.incr_r2_enable = incr_r2_enable;
        // end
        //   endcase
        end

      JMPUNC: case (unpack(instruction[27:26]))
        Jmp: begin
          pipe_id_out.val_r1 = zeroExtend(unpack(instruction[25:0]));
          pipe_id_out.val_r2 = 0;
          pipe_id_out.incr_r2_enable = False;
          pipe_id_out.jmpunc = True;
        end
        Jmpr: begin
          pipe_id_out.val_r1 = unpack(pc);
          pipe_id_out.val_r2 = offset_w;
          pipe_id_out.incr_r2_enable = incr_r2_enable;
          pipe_id_out.jmpunc = True;
        end
        Callr: begin
          pipe_id_out.val_r1 = unpack(pc);
          pipe_id_out.val_r2 = offset_w;
          pipe_id_out.val_dst = unpack(pc);
          pipe_id_out.incr_r2_enable = incr_r2_enable;
          pipe_id_out.jmpunc = True;
          pipe_id_out.write_reg = True;
        end
        Ret: begin
          pipe_id_out.val_r1 = pipe_id_out.val_dst;
          pipe_id_out.val_r2 = 0;
          pipe_id_out.incr_r2_enable = incr_r2_enable;
          pipe_id_out.jmpunc = True;
        end
      endcase

      JMPF: case (unpack(instruction[27:26]))
        Jmpz: begin
          pipe_id_out.val_r1 = unpack(pc);
          pipe_id_out.val_r2 = offset_w;
          pipe_id_out.incr_r2_enable = incr_r2_enable;
          pipe_id_out.jmpz = True;
        end
        Jmpnz: begin
          pipe_id_out.val_r1 = unpack(pc);
          pipe_id_out.val_r2 = offset_w;
          pipe_id_out.incr_r2_enable = incr_r2_enable;
          pipe_id_out.jmpnz = True;
        end
        Jmpc: begin
          pipe_id_out.val_r1 = unpack(pc);
          pipe_id_out.val_r2 = offset_w;
          pipe_id_out.incr_r2_enable = incr_r2_enable;
          pipe_id_out.jmpc = True;
        end
        Jmpnc: begin
          pipe_id_out.val_r1 = unpack(pc);
          pipe_id_out.val_r2 = offset_w;
          pipe_id_out.incr_r2_enable = incr_r2_enable;
          pipe_id_out.jmpnc = True;
        end
      endcase

      ALU: case (unpack(instruction[27:25]))
        And: begin
          pipe_id_out.write_reg = True;
          pipe_id_out.and_op = True;
        end
        Or: begin
          pipe_id_out.write_reg = True;
          pipe_id_out.or_op = True;
        end
        Xor: begin
          pipe_id_out.write_reg = True;
          pipe_id_out.xor_op = True;
        end
        Add: begin
          pipe_id_out.write_reg = True;
          pipe_id_out.add_op = True;
        end
        Mul: begin
          pipe_id_out.write_reg = True;
          pipe_id_out.mul_op = True;
        end
        Shl: begin
          pipe_id_out.write_reg = True;
          pipe_id_out.shl_op = True;
        end
        Shr: begin
          pipe_id_out.write_reg = True;
          pipe_id_out.shr_op = True;
        end
        Cmp: begin
          pipe_id_out.cmp_op = True;
        end
      endcase

      LDRF: case (unpack(instruction[27:26]))
        Ldrz: begin
          pipe_id_out.jmpz = True;
          pipe_id_out.ldrf_op = True;
          pipe_id_out.write_reg = True;
        end
        Ldrnz: begin
          pipe_id_out.jmpnz = True;
          pipe_id_out.ldrf_op = True;
          pipe_id_out.write_reg = True;
        end
        Ldrc: begin
          pipe_id_out.ldrf_op = True;
          pipe_id_out.jmpc = True;
          pipe_id_out.write_reg = True;
        end
        Ldrnc: begin
          pipe_id_out.ldrf_op = True;
          pipe_id_out.jmpnc = True;
          pipe_id_out.write_reg = True;
        end
      endcase

      default: pipe_id_out = defaultValue;
    endcase

    return pipe_id_out;
  endfunction

  rule feed_instructionFifo(instructionFifo.notEmpty && decodeFifo.notFull);
      Instruction i = instructionFifo.first;
      instructionFifo.deq;
      
      let pipe_id_out = decode(i, pipe_wb_mem, pipe_wb_ex);
      decodeFifo.enq(pipe_id_out);
  endrule

  method Action wb_ex_in(Pipe_s wb_ex);
    pipe_wb_ex <= wb_ex;
  endmethod

  method Action wb_mem_in(Pipe_s wb_mem);
    pipe_wb_mem <= wb_mem;  
  endmethod
endmodule
