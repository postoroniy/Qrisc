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
import Vector::*;

module qrisc32_RF(RF_IFC);
    Vector#(32, Reg#(Word32)) rf <- replicateM(mkRegA(0));

    method Action write_wb(Pipe_s ex, Pipe_s mem);
        Bool mem_writes = mem.write_reg;

        if (ex.write_reg && ex.incr_r2_enable && (ex.dst_r == ex.src_r2)) begin
            if (!(mem_writes && (mem.dst_r == ex.dst_r)))
                rf[ex.dst_r] <= ex.val_r2;
        end
        else begin
            if (ex.write_reg && !(mem_writes && (mem.dst_r == ex.dst_r)))
                rf[ex.dst_r] <= ex.val_dst;
            if (ex.incr_r2_enable && !(mem_writes && (mem.dst_r == ex.src_r2)))
                rf[ex.src_r2] <= ex.val_r2;
        end

        if (mem_writes) begin
            rf[mem.dst_r] <= mem.val_dst;
        end
    endmethod

    method Word32 read_r1(RegIdx addr) = rf[addr];
    method Word32 read_r2(RegIdx addr) = rf[addr];
    method Word32 read_dst(RegIdx addr) = rf[addr];
endmodule
