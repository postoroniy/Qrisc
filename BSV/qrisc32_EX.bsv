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
import Qrisc_pack::*;
import Vector::*;
import FIFOF :: *;

module qrisc32_EX#(
        FIFOF#(Pipe_s) decodeFifo,
        FIFOF#(Pipe_s) exeFifo
    ) (EX_ifc);

    Reg#(Bool)      flagZ                   <- mkRegA(False);
    Reg#(Bool)      flagC                   <- mkRegA(False);
    Reg#(Word32)    new_address_reg         <- mkRegA(0);

    rule ex_stage(decodeFifo.notEmpty && !exeFifo.notFull);
        let ex_in = decodeFifo.first;
        decodeFifo.deq;

        let ex_out_v = ex_in;
        Bit#(33) summ_result = zeroExtend(pack(ex_in.val_r1)) + zeroExtend(pack(ex_in.val_r2));
        Bit#(64) mult_result = zeroExtend(pack(ex_in.val_r1)) * zeroExtend(pack(ex_in.val_r2));
        Bit#(33) shfl_result = zeroExtend(pack(ex_in.val_r1)) << ex_in.val_r2;
        Bit#(33) shfr_result = {pack(ex_in.val_r1),1'b0 } >> ex_in.val_r2;
        Bool flagZ_v=False;
        Bool flagC_v=False;

        if(ex_in.ldrf_op) begin
            if(   (ex_in.jmpz  && flagZ)
                ||(ex_in.jmpnz && !flagZ)
                ||(ex_in.jmpc  && flagC)
                ||(ex_in.jmpnc && !flagC)
                )
                ex_out_v.val_dst = ex_in.val_r1;
            else
                ex_out_v.val_dst = ex_in.val_r2;
        end else if(ex_in.and_op) begin
            ex_out_v.val_dst = ex_in.val_r1 & ex_in.val_r2;
            flagZ_v = ex_out_v.val_dst==0;
        end else if(ex_in.or_op) begin
            ex_out_v.val_dst = ex_in.val_r1 | ex_in.val_r2;
            flagZ_v = ex_out_v.val_dst==0;
        end else if(ex_in.xor_op) begin
            ex_out_v.val_dst = ex_in.val_r1 ^ ex_in.val_r2;
            flagZ_v = ex_out_v.val_dst==0;
        end else if(   ex_in.add_op//simple addition operation
                || ex_in.jmpunc //jump unconditional calculate address = R1+R2(offset)
                //jump conditional calculate address = R1+R2(offset) and check flags
                || ex_in.jmpz || ex_in.jmpnz || ex_in.jmpc || ex_in.jmpnc)
        begin
            flagC_v             =   summ_result[32]==1;
            ex_out_v.val_dst    =   unpack(summ_result[31:0]);
            flagZ_v             =   ex_out_v.val_dst==0;
        end else if(ex_in.mul_op) begin
            flagC_v             =   mult_result[63:32]>0;
            ex_out_v.val_dst    =   unpack(mult_result[31: 0]);
            flagZ_v             =   ex_out_v.val_dst==0;
        end else if(ex_in.shl_op) begin
            flagC_v             =   shfl_result[32]==1;
            ex_out_v.val_dst    =   unpack(shfl_result[31: 0]);
            flagZ_v             =   ex_out_v.val_dst==0;
        end else if(ex_in.shr_op) begin
            ex_out_v.val_dst    =   unpack(shfr_result[32:1]);
            flagC_v             =   shfr_result[0]==1;
            flagZ_v             =   ex_out_v.val_dst==0;
        end else if(ex_in.cmp_op) begin
            flagZ_v             =   ex_out_v.val_r1==ex_out_v.val_r2;
            flagC_v             =   !(ex_out_v.val_r1>=ex_out_v.val_r2);
        end
        let r2      = ex_in.val_r2;
        let inc_r2  = signExtend(ex_in.incr_r2);
        let r2_add  = r2 + inc_r2;

        ex_out_v.val_r2 = (ex_out_v.incr_r2_enable)?r2_add:ex_out_v.val_r2;

        if (ex_out_v.and_op || ex_out_v.or_op || ex_out_v.xor_op || ex_out_v.add_op ||
            ex_out_v.mul_op || ex_out_v.shl_op || ex_out_v.shr_op || ex_out_v.cmp_op)begin
            flagZ   <= flagZ_v;
            flagC   <= flagC_v;
        end
    
        if(ex_out_v.read_mem || ex_out_v.write_mem)
            ex_out_v.val_r1 = unpack(truncate(summ_result));//address for accessing
    
        if  (ex_out_v.jmpunc                ||
            (ex_out_v.jmpz && flagZ_v)      ||
            (ex_out_v.jmpnz && !flagZ_v)    ||
            (ex_out_v.jmpc && flagC_v)      ||
            (ex_out_v.jmpnc && !flagC_v) )
        begin
            new_address_reg <= pack(ex_out_v.val_dst);
        end    
        exeFifo.enq(ex_out_v);
    endrule

    method ActionValue#(Word32) get_new_address();
            return new_address_reg;
    endmethod
endmodule
