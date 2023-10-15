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
import Qrisc_pack::*;

module qrisc32_IF(IF_ifc);
    Wire#(Bool)     if_stall_w          <- mkBypassWire;
    Wire#(Bool)     if_wait_req_w       <- mkBypassWire;
    //
    Wire#(Bit#(32)) new_address_w       <- mkBypassWire;
    Reg#(Bit#(32))  data_r              <- mkReg(0);
    Reg#(Bit#(32))  address_r           <- mkReg(0);
    Wire#(Bool)     new_address_valid_w <- mkBypassWire;
    //
    Reg#(Bit#(32))  stalled_adr0        <- mkReg(0);
    // Reg#(Bit#(32))  stalled_adr1        <- mkReg(0);

    let all_stall_w    =   if_stall_w || if_wait_req_w;
    let offset_w       = (all_stall_w || new_address_valid_w)? 0:4;
    let base_w         = (new_address_valid_w)? new_address_w : address_r;
    let jump_address_w = base_w + offset_w;

    let pc_w = (all_stall_w)? stalled_adr0 : jump_address_w;

    (* no_implicit_conditions *)
    rule every;
      if (!all_stall_w)
      begin
        stalled_adr0 <= jump_address_w;
        // stalled_adr1 <= stalled_adr0;
      end
      address_r <= pc_w;
    endrule

  interface AvalonMasterReadIfc avm_instructions;
    method Action check_wait(wait_req);
      if_wait_req_w <= wait_req;
    endmethod

    method Action get_data(Bit#(32) data);
      if(!if_stall_w && !if_wait_req_w)
        data_r <= data;
    endmethod

    method Bit#(32) addr();
      return address_r;
    endmethod

    method Bool rd();
      return True;
    endmethod
  endinterface

  method Action check_pipe_stall(pipe_stall);
    if_stall_w <= pipe_stall;
  endmethod

  method Action set_new_address(new_address,new_address_valid);
    new_address_w <= new_address;
    new_address_valid_w <= new_address_valid;
  endmethod

  method ActionValue#(Bit#(32)) instruction;
    return data_r;
  endmethod

  method ActionValue#(Bit#(32)) pc;
    return address_r;
  endmethod
endmodule
