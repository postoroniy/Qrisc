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
    Vector#(32, Reg#(Bit#(32))) rf <- replicateM(mkReg(0));

    method Action writeReg(Bit#(5) addr, Bit#(32) data);
        if (addr != 0) rf[addr] <= data; // Register 0 typically hardwired to 0
    endmethod

    method Bit#(32) readReg1(Bit#(5) addr) = rf[addr];
    method Bit#(32) readReg2(Bit#(5) addr) = rf[addr];
endmodule
