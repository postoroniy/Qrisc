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

    Reg#(Bool)          flagZ <- mkRegA(False);
    Reg#(Bool)          flagC <- mkRegA(False);
    Reg#(MaybeWord32)   new_address_reg <- mkRegA(Invalid);

    (* descending_urgency = "ex_stage, clear_redirect" *)
    rule clear_redirect;
        new_address_reg <= Invalid;
    endrule

    rule ex_stage(decodeFifo.notEmpty && exeFifo.notFull && wbExFifo.notFull);
        let ex_in = decodeFifo.first;
        decodeFifo.deq;

        let ex_out = ex_in;
        MaybeWord32 new_addr = Invalid;

        Bit#(33) summ = zeroExtend(ex_in.val_r1) + zeroExtend(ex_in.val_r2);
        Bit#(33) shl  = zeroExtend(ex_in.val_r1) << ex_in.val_r2;
        Bit#(33) shr  = {ex_in.val_r1, 1'b0} >> ex_in.val_r2;
        Bit#(64) prod = zeroExtend(ex_in.val_r1) * zeroExtend(ex_in.val_r2);

        Bool flagZ_w = False;
        Bool flagC_w = False;

        if (ex_in.ldrf_op) begin
            if (   (ex_in.jmpz  && flagZ)
                || (ex_in.jmpnz && !flagZ)
                || (ex_in.jmpc  && flagC)
                || (ex_in.jmpnc && !flagC)) begin
                ex_out.val_dst = ex_in.val_r1;
            end
            else begin
                ex_out.val_dst = ex_in.val_r2;
            end
        end
        else if (ex_in.and_op) begin
            ex_out.val_dst = ex_in.val_r1 & ex_in.val_r2;
            flagZ_w = (ex_out.val_dst == 0);
            flagC_w = False;
        end
        else if (ex_in.or_op) begin
            ex_out.val_dst = ex_in.val_r1 | ex_in.val_r2;
            flagZ_w = (ex_out.val_dst == 0);
            flagC_w = False;
        end
        else if (ex_in.xor_op) begin
            ex_out.val_dst = ex_in.val_r1 ^ ex_in.val_r2;
            flagZ_w = (ex_out.val_dst == 0);
            flagC_w = False;
        end
        else if (ex_in.add_op || ex_in.jmpunc || ex_in.jmpz || ex_in.jmpnz || ex_in.jmpc || ex_in.jmpnc) begin
            ex_out.val_dst = truncate(summ);
            flagZ_w = (ex_out.val_dst == 0);
            flagC_w = (summ[32] == 1);
        end
        else if (ex_in.mul_op) begin
            ex_out.val_dst = truncate(prod);
            flagZ_w = (ex_out.val_dst == 0);
            flagC_w = False;
        end
        else if (ex_in.shl_op) begin
            ex_out.val_dst = shl[31:0];
            flagZ_w = (ex_out.val_dst == 0);
            flagC_w = False;
        end
        else if (ex_in.shr_op) begin
            ex_out.val_dst = shr[32:1];
            flagZ_w = (ex_out.val_dst == 0);
            flagC_w = (shr[0] == 1);
        end
        else if (ex_in.cmp_op) begin
            flagZ_w = (ex_out.val_r1 == ex_out.val_r2);
            flagC_w = (ex_out.val_r1 >= ex_out.val_r2) ? False : True;
        end

        if (ex_out.incr_r2_enable) begin
            Int#(32) r2 = unpack(ex_out.val_r2);
            Int#(32) inc = extend(ex_out.incr_r2);
            ex_out.val_r2 = pack(r2 + inc);
        end

        if (ex_in.read_mem || ex_in.write_mem) begin
            ex_out.val_r1 = truncate(summ);
        end

        if (ex_in.and_op || ex_in.or_op || ex_in.xor_op || ex_in.add_op
            || ex_in.mul_op || ex_in.shl_op || ex_in.shr_op || ex_in.cmp_op) begin
            flagZ <= flagZ_w;
            flagC <= flagC_w;
        end

        if (ex_in.jmpunc
            || (ex_in.jmpz  && flagZ)
            || (ex_in.jmpnz && !flagZ)
            || (ex_in.jmpc  && flagC)
            || (ex_in.jmpnc && !flagC)) begin
            if (!ex_in.ldrf_op) begin
                new_addr = tagged Valid ex_out.val_dst;
            end
        end
        new_address_reg <= new_addr;

        exeFifo.enq(ex_out);
        wbExFifo.enq(ex_out);
    endrule

    method MaybeWord32 get_new_address();
        return new_address_reg;
    endmethod

endmodule
