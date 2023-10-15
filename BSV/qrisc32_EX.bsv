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
import Vector::*;
import Qrisc_pack::*;

module qrisc32_EX(EX_ifc);
    //
    // input  logic                  clk,reset,pipe_stall,
    // input  risc_pack::pipe_struct ex_in,
    // output risc_pack::pipe_struct pipe_ex_out,
    //
    // output bit          new_address_valid,//to mem stage
    // output bit[31:0]    new_address//to mem stage
    // );

    Wire#(Pipe_s) ex_out_w  <- mkBypassWire;
    Wire#(Bool)   flagZ_w  <- mkBypassWire;
    Wire#(Bool)   flagC_w  <- mkBypassWire;
    Reg#(Bool)    flagZ    <- mkReg(False);
    Reg#(Bool)    flagC    <- mkReg(False);
    // wire signed[31:0]   r2      = ex_in.val_r2;
    // wire signed[3:0]    inc_r2  = ex_in.incr_r2;
    // wire signed[31:0]   r2_add  = r2 + inc_r2;
    //

    method Action ex_stage_in(Pipe_s ex_in);
      Pipe_s  ex_out_v = ex_in;
      summ_result = ex_in.val_r1 + ex_in.val_r2;
      Bool flagZ_v=False;
      Bool flagC_v=False;

      if(ex_in.ldrf_op)
      begin
          if(   (ex_in.jmpz  && flagZ)
              ||(ex_in.jmpnz && ~flagZ)
              ||(ex_in.jmpc  && flagC)
              ||(ex_in.jmpnc && ~flagC)
              )
              ex_out_v.val_dst=ex_in.val_r1;
          else
              ex_out_v.val_dst=ex_in.val_r2;
      end
      else if(ex_in.and_op)
      begin
          ex_out_v.val_dst=ex_in.val_r1 & ex_in.val_r2;
          flagC_v=False;
          flagZ_v=ex_out_v.val_dst==0;
      end
      else if(ex_in.or_op)
      begin
          ex_out_v.val_dst=ex_in.val_r1 | ex_in.val_r2;
          flagC_v=False;
          flagZ_v=ex_out_v.val_dst==0;
      end
      else if(ex_in.xor_op)
      begin
          ex_out_v.val_dst=ex_in.val_r1 ^ ex_in.val_r2;
          flagC_v=False;
          flagZ_v=ex_out_v.val_dst==0;
      end
      else if(   ex_in.add_op//simple addition operation
              || ex_in.jmpunc //jump unconditional calculate address = R1+R2(offset)
              //jump conditional calculate address = R1+R2(offset) and check flags
              || ex_in.jmpz || ex_in.jmpnz || ex_in.jmpc || ex_in.jmpnc)
      begin
          // {flagC_v,ex_out_v.val_dst}=summ_result;
          flagZ_v=ex_out_v.val_dst==0;
      end
      else if(ex_in.mul_op)
      begin
          {flagC_v,ex_out_v.val_dst}=ex_in.val_r1 * ex_in.val_r2;
          flagZ_v=ex_out_v.val_dst==0;
      end
      else if(ex_in.shl_op)
      begin
          {flagC_v,ex_out_v.val_dst}=ex_in.val_r1 << ex_in.val_r2;
          flagZ_v=ex_out_v.val_dst==0;
      end
      else if(ex_in.shr_op)
      begin
          {ex_out_v.val_dst,flagC_v}={ex_in.val_r1,1'b0 } >> ex_in.val_r2;
          flagZ_v=ex_out_v.val_dst==0;
      end
      else if(ex_in.cmp_op)
      begin
          flagZ_v=ex_out_v.val_r1==ex_out_v.val_r2;
          flagC_v=!(ex_out_v.val_r1>=ex_out_v.val_r2);
      end
      ex_out_v.val_r2 = (ex_out_v.incr_r2_enable)?r2_add:ex_out_v.val_r2;

      ex_out_w <= ex_out_v;
      flagZ_w <= flagZ_v;
      flagC_w <= flagC_v;
    endmethod
    //
    // always@(posedge clk)
    // begin
    //     flagZ<=
    //     (ex_in.and_op | ex_in.or_op | ex_in.xor_op | ex_in.add_op
    //     | ex_in.mul_op | ex_in.shl_op | ex_in.shr_op | ex_in.cmp_op)?flagZ_v:flagZ;
    //
    //     flagC<=
    //     (ex_in.and_op | ex_in.or_op | ex_in.xor_op | ex_in.add_op
    //     | ex_in.mul_op | ex_in.shl_op | ex_in.shr_op | ex_in.cmp_op)?flagC_v:flagC;
    //
    //     if(ex_in.jmpunc || (ex_in.jmpz & flagZ) ||(ex_in.jmpnz & !flagZ)||
    //         (ex_in.jmpc & flagC) ||(ex_in.jmpnc & !flagC) )
    //     begin
    //         new_address_valid<=~ex_in.ldrf_op;
    //         new_address<=ex_out_w.val_dst;
    //     end
    //     else
    //         new_address_valid<=0;
    //
    //     if(~pipe_stall)
    //     begin
    //         pipe_ex_out<=ex_out_w;
    //         if(ex_in.read_mem| ex_in.write_mem)
    //             pipe_ex_out.val_r1<=summ_result;//address for accessing
    //     end
    // end
endmodule
