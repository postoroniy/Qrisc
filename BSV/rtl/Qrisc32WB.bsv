//////////////////////////////////////////////////////////////////////////////////////////////
//    Project Qrisc32 is risc cpu implementation, purpose is studying
//    Digital System Design course at Kyoung Hee University during my PhD earning
//    Copyright (C) 2010-2026  Viacheslav Vinogradov
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
//////////////////////////////////////////////////////////////////////////////////////////////
import Qrisc_pack::*;
import FIFOF::*;
import RWire::*;

module qrisc32_WB#(
        FIFOF#(Pipe_s) wbExFifo,
        FIFOF#(Pipe_s) wbMemFifo,
        RF_IFC rf
    ) (WB_IFC);

    Reg#(Pipe_s) wb_ex_reg  <- mkRegA(defaultValue);
    Reg#(Pipe_s) wb_mem_reg <- mkRegA(defaultValue);

    RWire#(Pipe_s) wb_ex_bypass  <- mkRWire;
    RWire#(Pipe_s) wb_mem_bypass <- mkRWire;

    rule writeback(wbExFifo.notEmpty || wbMemFifo.notEmpty);
        Bool have_ex = wbExFifo.notEmpty;
        Bool have_mem = wbMemFifo.notEmpty;

        Pipe_s ex = defaultValue;
        Pipe_s mem = defaultValue;

        if (have_ex) begin
            ex = wbExFifo.first;
            wbExFifo.deq;
            wb_ex_reg <= ex;
            wb_ex_bypass.wset(ex);
        end

        if (have_mem) begin
            mem = wbMemFifo.first;
            wbMemFifo.deq;
            wb_mem_reg <= mem;
            wb_mem_bypass.wset(mem);
        end

        rf.write_wb(ex, mem);
    endrule

    method Pipe_s forward_ex();
        Pipe_s fwd = wb_ex_reg;
        let bypass = wb_ex_bypass.wget;
        if (isValid(bypass)) begin
            fwd = validValue(bypass);
        end
        return fwd;
    endmethod

    method Pipe_s forward_mem();
        Pipe_s fwd = wb_mem_reg;
        let bypass = wb_mem_bypass.wget;
        if (isValid(bypass)) begin
            fwd = validValue(bypass);
        end
        return fwd;
    endmethod
endmodule
