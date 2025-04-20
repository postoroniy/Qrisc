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
        FIFOF#(Pipe_s) exeFifo,
        FIFOF#(Pipe_s) wbExFifo
    ) (EX_ifc);

    Reg#(Bit#(32))  mult1_reg         <- mkRegA(0);
    Reg#(Bit#(32))  mult2_reg         <- mkRegA(0);
    let mult_result = mult1_reg * mult2_reg;
    Reg#(MaybePipe_s) exeMult <- mkRegA(Invalid);

    //
    Reg#(Bool)      flagZ                   <- mkRegA(False);
    Reg#(Bool)      flagC                   <- mkRegA(False);
    Reg#(Word32)    new_address_reg         <- mkRegA(0);

    rule ex_stage1(decodeFifo.notEmpty && exeFifo.notFull && !isValid(exeMult));
        let ex_in = decodeFifo.first;
        decodeFifo.deq;

        let ex_out_v = ex_in;
        Bit#(33) summ_result = zeroExtend(pack(ex_in.val_r1)) + zeroExtend(pack(ex_in.val_r2));
        Bit#(33) shfl_result = zeroExtend(pack(ex_in.val_r1)) << ex_in.val_r2;
        Bit#(33) shfr_result = {pack(ex_in.val_r1),1'b0 } >> ex_in.val_r2;

        if(ex_in.and_op) begin
            ex_out_v.val_dst = ex_in.val_r1 & ex_in.val_r2;
        end else if(ex_in.or_op) begin
            ex_out_v.val_dst = ex_in.val_r1 | ex_in.val_r2;
        end else if(ex_in.xor_op) begin
            ex_out_v.val_dst = ex_in.val_r1 ^ ex_in.val_r2;
        end else if(ex_in.add_op || ex_in.pc_change_op)begin
            ex_out_v.val_dst    =   unpack(summ_result[31:0]);
        end else if(ex_in.mul_op) begin
            mult1_reg <= ex_in.val_r1;
            mult2_reg <= ex_in.val_r2;
        end else if(ex_in.shl_op) begin
            ex_out_v.val_dst    =   unpack(shfl_result[31: 0]);
        end else if(ex_in.shr_op) begin
            ex_out_v.val_dst    =   unpack(shfr_result[32:1]);
        end

        let incr_val = case (ex_in.incr2)
                        Incr1:  1;
                        Incr2:  2;
                        Incr4:  4;
                        Decr1: -1;
                        Decr2: -2;
                        Decr4: -4;
                        default: 0;
                    endcase;

        Int#(32) incr_val_sext = fromInteger(incr_val);
        ex_out_v.val_r2 = unpack(pack(ex_in.val_r2) + pack(incr_val_sext));

        if(!ex_in.mul_op)begin
            exeFifo.enq(ex_out_v);
            wbExFifo.enq(ex_out_v);
        end else begin
            exeMult <= tagged Valid ex_out_v;
        end
    endrule

    rule ex_stage2(exeFifo.notFull && isValid(exeMult));
        let ex_in = validValue(exeMult);
        let ex_out_v = ex_in;
        ex_out_v.val_dst = unpack(mult_result[31:0]);

        Bool flagZ_v=flagZ;
        Bool flagC_v=flagC;

        flagC <= mult_result[63:32] > 0;
        flagZ <= mult_result[31:0] == 0;

    //     // Bit#(33) summ_result = zeroExtend(pack(ex_in.val_r1)) + zeroExtend(pack(ex_in.val_r2));
    //     // Bit#(64) mult_result = zeroExtend(pack(ex_in.val_r1)) * zeroExtend(pack(ex_in.val_r2));
    //     // Bit#(33) shfl_result = zeroExtend(pack(ex_in.val_r1)) << ex_in.val_r2;
    //     // Bit#(33) shfr_result = {pack(ex_in.val_r1),1'b0 } >> ex_in.val_r2;


    //     if(ex_in.and_op) begin
    //     //     ex_out_v.val_dst = ex_in.val_r1 & ex_in.val_r2;
    //         flagZ_v = ex_out_v.val_dst==0;
    //     end else if(ex_in.or_op) begin
    //     //     ex_out_v.val_dst = ex_in.val_r1 | ex_in.val_r2;
    //         flagZ_v = ex_out_v.val_dst==0;
    //     end else if(ex_in.xor_op) begin
    //     //     ex_out_v.val_dst = ex_in.val_r1 ^ ex_in.val_r2;
    //         flagZ_v = ex_out_v.val_dst==0;
    //     end else if(ex_in.add_op || ex_in.pc_change_op)begin
    //         flagC_v             =   summ_result[32]==1;
    //     //     ex_out_v.val_dst    =   unpack(summ_result[31:0]);
    //         flagZ_v             =   ex_out_v.val_dst==0;
    //     end else if(ex_in.mul_op) begin
    //     //     flagC_v             =   mult_result[63:32]>0;
    //     //     ex_out_v.val_dst    =   unpack(mult_result[31: 0]);
    //     //     flagZ_v             =   ex_out_v.val_dst==0;
    //     // end else if(ex_in.shl_op) begin
    //     //     flagC_v             =   shfl_result[32]==1;
    //     //     ex_out_v.val_dst    =   unpack(shfl_result[31: 0]);
    //     //     flagZ_v             =   ex_out_v.val_dst==0;
    //     // end else if(ex_in.shr_op) begin
    //     //     ex_out_v.val_dst    =   unpack(shfr_result[32:1]);
    //     //     flagC_v             =   shfr_result[0]==1;
    //     //     flagZ_v             =   ex_out_v.val_dst==0;
    //     // end else if(ex_in.cmp_op) begin
    //     //     flagZ_v             =   ex_out_v.val_r1==ex_out_v.val_r2;
    //     //     flagC_v             =   !(ex_out_v.val_r1>=ex_out_v.val_r2);
    //     // end else  if(ex_in.write_reg) begin
    //     //     if( (!ex_out_v.check_z && !ex_out_v.check_nz && !ex_out_v.check_c && ex_out_v.check_nc) ||
    //     //         (
    //     //             (ex_out_v.check_z && flagZ_v)   ||
    //     //             (ex_out_v.check_nz && !flagZ_v)    ||
    //     //             (ex_out_v.check_c && flagC_v)      ||
    //     //             (ex_out_v.check_nc && !flagC_v)
    //     //         )
    //     //     )
    //     //         ex_out_v.val_dst = ex_in.val_r1;
    //     //     else
    //     //         ex_out_v.val_dst = ex_in.val_r2;
    //     // end

    //     // let incr_val = case (ex_in.incr2)
    //     //                 Incr1:  1;
    //     //                 Incr2:  2;
    //     //                 Incr4:  4;
    //     //                 Decr1: -1;
    //     //                 Decr2: -2;
    //     //                 Decr4: -4;
    //     //                 default: 0;
    //     //             endcase;

    //     // Int#(32) incr_val_sext = fromInteger(incr_val);
    //     // ex_out_v.val_r2 = unpack(pack(ex_in.val_r2) + pack(incr_val_sext));

    //     // flagZ   <= flagZ_v;
    //     // flagC   <= flagC_v;

    //     // if(ex_out_v.read_mem || ex_out_v.write_mem)
    //     //     ex_out_v.val_r1 = unpack(truncate(summ_result));//address for accessing

    //     // if  (ex_out_v.pc_change_op  &&
    //     //         (
    //     //             (!ex_out_v.check_z && !ex_out_v.check_nz && !ex_out_v.check_c && ex_out_v.check_nc) ||
    //     //             (
    //     //                 (ex_out_v.check_z && flagZ_v)   ||
    //     //                 (ex_out_v.check_nz && !flagZ_v)    ||
    //     //                 (ex_out_v.check_c && flagC_v)      ||
    //     //                 (ex_out_v.check_nc && !flagC_v)
    //     //             )
    //     //         )
    //     //     )
    //     // begin
    //     //     new_address_reg <= pack(ex_out_v.val_dst);
    //     // end

        exeMult <= Invalid;
        exeFifo.enq(ex_out_v);
        wbExFifo.enq(ex_out_v);
    endrule

    method ActionValue#(Word32) get_new_address();
        return new_address_reg;
    endmethod

endmodule
