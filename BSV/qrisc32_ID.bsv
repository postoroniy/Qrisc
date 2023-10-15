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

module qrisc32_ID(ID_ifc);

    Vector#(32, Reg#(Bit#(32)))         rf  <- replicateM(mkReg(0));
    Wire#(Pipe_s)                 id_out_w  <- mkBypassWire;
    Reg#(Pipe_s)                  id_out_r  <- mkRegU;

    Reg#(Bit#(32))    nop_counter <-mkReg(0);
    Reg#(Bit#(32))    jmp_counter <-mkReg(0);
    Reg#(Bit#(32))    alu_counter <-mkReg(0);
    Reg#(Bit#(32))    oth_counter <-mkReg(0);

    method Action id_in(
      Bit#(32) instruction,
      Bit#(32) pc,
      Pipe_s wb_mem,
      Pipe_s wb_ex);

      Pipe_s  id_out_v;
      id_out_v.dst_r     = instruction[04:00];
      id_out_v.src_r1    = instruction[09:05];
      id_out_v.src_r2    = instruction[14:10];

      id_out_v.incr_r2   = 0;
      id_out_v.incr_r2_enable    = True;

      id_out_v.read_mem  = False;
      id_out_v.write_mem = False;
      id_out_v.write_reg = False;

      id_out_v.mul_op    = False;
      id_out_v.add_op    = False;
      id_out_v.or_op     = False;
      id_out_v.and_op    = False;
      id_out_v.xor_op    = False;
      id_out_v.shl_op    = False;
      id_out_v.shr_op    = False;
      id_out_v.cmp_op    = False;
      id_out_v.ldrf_op   = False;

      id_out_v.jmpunc    = False;
      id_out_v.jmpz      = False;
      id_out_v.jmpnz     = False;
      id_out_v.jmpc      = False;
      id_out_v.jmpnc     = False;

      id_out_v.val_r1    =  (wb_mem.write_reg && wb_mem.dst_r==id_out_v.src_r1)?
                                  wb_mem.val_dst://forward from memory read
                                  //(wb_mem.incr_r2_enable && wb_mem.src_r2==id_out_v.src_r1)?
                                  //wb_mem.val_r2://forward from mem stage R2 register
                                  (wb_ex.write_reg && wb_ex.dst_r==id_out_v.src_r1)?
                                  wb_ex.val_dst://forward from execution stage DST register
                                  (wb_ex.incr_r2_enable && wb_ex.src_r2==id_out_v.src_r1)?
                                  wb_ex.val_r2://forward from execution stage R2 register
                                  rf[id_out_v.src_r1];//otherwise from register file

      id_out_v.val_r2    =  (wb_mem.write_reg && wb_mem.dst_r==id_out_v.src_r2)?
                                  wb_mem.val_dst://forward from memory read
                                  //(wb_mem.incr_r2_enable && wb_mem.src_r2==id_out_v.src_r2)?
                                  //wb_mem.val_r2://forward from mem stage R2 register
                                  (wb_ex.write_reg && wb_ex.dst_r==id_out_v.src_r2)?
                                  wb_ex.val_dst://forward from execution stage DST register
                                  (wb_ex.incr_r2_enable && wb_ex.src_r2==id_out_v.src_r2)?
                                  wb_ex.val_r2://forward from execution stage R2 register
                                  rf[id_out_v.src_r2];//otherwise from register file

      id_out_v.val_dst    = (wb_mem.write_reg && wb_mem.dst_r==id_out_v.dst_r)?
                                  wb_mem.val_dst://forward from memory read
                                  //(wb_mem.incr_r2_enable && wb_mem.src_r2==id_out_v.dst_r)?
                                  //wb_mem.val_r2://forward from mem stage R2 register
                                  (wb_ex.write_reg && wb_ex.dst_r==id_out_v.dst_r)?
                                  wb_ex.val_dst://forward from execution stage DST register
                                  (wb_ex.incr_r2_enable && wb_ex.src_r2==id_out_v.dst_r)?
                                  wb_ex.val_r2://forward from execution stage R2 register
                                  rf[id_out_v.dst_r];//otherwise from register file

      let offset_w = (instruction[25]==1)? id_out_v.val_r2 : signExtend( pack( instruction[24:10] ) );//17 bit sign + 15 bit offset

      case(instruction[24:22]) matches
          0:begin id_out_v.incr_r2=4'd0;id_out_v.incr_r2_enable = False;end
          1:id_out_v.incr_r2=4'd1;
          2:id_out_v.incr_r2=4'd2;
          3:id_out_v.incr_r2=4'd4;
          4:begin id_out_v.incr_r2=4'd0;id_out_v.incr_r2_enable = False;end
          5:id_out_v.incr_r2=-4'd1;
          6:id_out_v.incr_r2=-4'd2;
          7:id_out_v.incr_r2=-4'd4;
      endcase

      case(unpack(instruction[31:28])) matches
          //load and store
          LDR:
          case(instruction[27:26]) matches
              2'b00:
              begin
                id_out_v.write_reg = id_out_v.dst_r!=id_out_v.src_r1;//write from reg src1 to reg dst
                //id_out_v.incr_r2_enable    = id_out_v.write_reg;
                id_out_v.val_dst = id_out_v.val_r1;
              end
              2'b01:
              begin
                  id_out_v.val_dst[31:16]= instruction[20:5];
                  id_out_v.write_reg     = True;//write from reg src1 to reg dst
                  id_out_v.incr_r2_enable= False;
              end
              2'b10:
              begin
                  id_out_v.val_dst[15:0] = instruction[20:5];
                  id_out_v.write_reg     = True;//write from reg src1 to reg dst
                  id_out_v.incr_r2_enable= False;
              end
              2'b11:
              begin
                  id_out_v.read_mem      = True;//read from mem by Rscr2+Rsrc1 and then write to register
                  id_out_v.write_reg     = True;//write from reg src1 to reg dst
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
                  id_out_v.val_r2        = offset_w;
              end
          endcase

          STR:
          case(instruction[27:26]) matches
              //2'b11:
              default:
              begin
                  id_out_v.write_mem     = True;//write Rdst to mem by Rscr2+Rsrc1
                  id_out_v.val_r2        = offset_w;
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
              end
          endcase

          //jumps
          JMPUNC:
          case(instruction[27:26]) matches
              2'b00:
              begin
                  id_out_v.val_r1        = zeroExtend(instruction[25:0]);
                  id_out_v.val_r2        = 0;//no offset
                  id_out_v.incr_r2_enable= False;
                  id_out_v.jmpunc        = True;
              end
              2'b01://relative jump
              begin
                  id_out_v.val_r1        = pc;
                  id_out_v.val_r2        = offset_w;//offset
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
                  id_out_v.jmpunc        = True;
              end
              2'b10://call
              begin
                  id_out_v.val_r1        = pc;
                  id_out_v.val_r2        = offset_w;//offset
                  id_out_v.val_dst       = pc;//return address
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
                  id_out_v.jmpunc        = True;
                  id_out_v.write_reg     = True;
              end
              2'b11://ret
              begin
                  id_out_v.val_r1        = id_out_v.val_dst;
                  id_out_v.val_r2        = 0;//offset
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
                  id_out_v.jmpunc        = True;
              end
          endcase

          JMPF:
          case(instruction[27:26]) matches
              2'b00://jmpz
              begin
                  id_out_v.val_r1        = pc;
                  id_out_v.val_r2        = offset_w;//offset
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
                  id_out_v.jmpz          = True;
              end
              2'b01://jmpnz
              begin
                  id_out_v.val_r1        = pc;
                  id_out_v.val_r2        = offset_w;//offset
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
                  id_out_v.jmpnz         = True;
              end
              2'b10://jmpc
              begin
                  id_out_v.val_r1        = pc;
                  id_out_v.val_r2        = offset_w;//offset
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
                  id_out_v.jmpc          = True;
              end
              2'b11://jmpnc
              begin
                  id_out_v.val_r1        = pc;
                  id_out_v.val_r2        = offset_w;//offset
                  id_out_v.incr_r2_enable= unpack(instruction[25]);
                  id_out_v.jmpnc         = True;
              end
          endcase

          //Arithmetic
          ALU:
          case(instruction[27:25]) matches
              3'd0:
              begin
                  id_out_v.write_reg = True;
                  id_out_v.and_op    = True;
              end
              3'd1:
              begin
                  id_out_v.write_reg = True;
                  id_out_v.or_op     = True;
              end
              3'd2:
              begin
                  id_out_v.write_reg = True;
                  id_out_v.xor_op    = True;
              end
              3'd3:
              begin
                  id_out_v.write_reg = True;
                  id_out_v.add_op    = True;
              end
              3'd4:
              begin
                  id_out_v.write_reg = True;
                  id_out_v.mul_op    = True;
              end
              3'd5:
              begin
                  id_out_v.write_reg = True;
                  id_out_v.shl_op    = True;
              end
              3'd6:
              begin
                  id_out_v.write_reg = True;
                  id_out_v.shr_op    = True;
              end
              //cmp_op =7
              default:
              begin
                  //id_out_v.write_reg    = False;
                  id_out_v.cmp_op    = True;
              end
          endcase

          LDRF:
          case(instruction[27:26]) matches
              2'b00://ldrz
              begin
                  id_out_v.jmpz      = True;
                  id_out_v.ldrf_op   = True;
                  id_out_v.write_reg = True;
              end
              2'b01://ldrnz
              begin
                  id_out_v.jmpnz     = True;
                  id_out_v.ldrf_op   = True;
                  id_out_v.write_reg = True;
              end
              2'b10://ldrc
              begin
                  id_out_v.ldrf_op   = True;
                  id_out_v.jmpc      = True;
                  id_out_v.write_reg = True;
              end
              2'b11://ldrnc
              begin
                  id_out_v.ldrf_op   = True;
                  id_out_v.jmpnc     = True;
                  id_out_v.write_reg = True;
              end
          endcase

          default:
              begin
                  // id_out_v=0;
              end
      endcase

      id_out_w <= id_out_v;

      if(wb_ex.write_reg)//from ex stage DST register
          rf[wb_ex.dst_r] <= wb_ex.val_dst;

      if(wb_ex.incr_r2_enable)//from ex stage R2 register
        if(!wb_ex.write_reg || ( wb_ex.write_reg && wb_ex.dst_r != wb_ex.src_r2))
          rf[wb_ex.src_r2] <= wb_ex.val_r2;

      if(wb_mem.write_reg)//from memory read stage
        if(!wb_ex.write_reg || ( wb_ex.write_reg && wb_ex.dst_r != wb_mem.dst_r))
          if(!wb_ex.incr_r2_enable || ( wb_ex.incr_r2_enable && wb_ex.src_r2 != wb_mem.dst_r))
            rf[wb_mem.dst_r] <= wb_mem.val_dst;

      //if(wb_mem.incr_r2_enable)//from mem stage R2 register
      //    rf[wb_mem.src_r2]<=wb_mem.val_r2;

      if(instruction==0)
          nop_counter<=nop_counter+1;
      else
      if(unpack(instruction[31:28])==JMPUNC || unpack(instruction[31:28])==JMPF)
          jmp_counter<=jmp_counter+1;
      else
      if(unpack(instruction[31:28])==ALU)
          alu_counter<=alu_counter+1;
      else
          oth_counter<=oth_counter+1;

    endmethod

    method ActionValue#(Pipe_s) id_out(Bool pipe_stall);
      if(!pipe_stall)
        id_out_r <= id_out_w;
      return id_out_r;
    endmethod

//
//     always@(posedge clk)// or posedge reset)
//     if(reset)
//     begin
//         pipe_id_out<='0;
//     end
//     else
//     begin
//
// //synthesys translate_off
//         if(verbose)
//         begin
//             if(~pipe_stall)
//             begin
//                 case(instruction[31:28])
//                     //load and store
//                     risc_pack::LDR:
//                     case(instruction[27:26])
//                         2'b00:$display("LDR R%0d, R%0d, R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                         2'b01:$display("LDRH R%0d,0x%x",id_out_w.dst_r,instruction[20:5]);
//                         2'b10:$display("LDRL R%0d,0x%x",id_out_w.dst_r,instruction[20:5]);
//                         2'b11:$display("LDRP R%0d,[R%0d +%0d],R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,$signed(offset_w),id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     endcase
//
//                 risc_pack::STR:
//                 case(instruction[27:26])
//                     default:$display("STR R%0d,[R%0d +%0d],R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,$signed(offset_w),id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                 endcase
//
//                 //jumps
//                 risc_pack::JMPUNC:
//                 case(instruction[27:26])
//                     2'b00:$display("JMP 0x%0x",instruction[25:0]);
//                     2'b01://relative jump
//                     $display("JMPR PC 0x%0x + offset %0d",pc,$signed(offset_w));
//                     2'b10://call
//                     $display("CALLR PC 0x%0x + offset %0d",pc,$signed(offset_w));
//                     2'b11://ret
//                     $display("RET to 0x%0x",id_out_w.val_dst);
//                 endcase
//
//                 risc_pack::JMPF:
//                 case(instruction[27:26])
//                     2'b00://jmpz
//                         $display("JMPZ PC 0x%0x + offset %0d",pc,$signed(offset_w));
//                     2'b01://jmpnz
//                         $display("JMPNZ PC 0x%0x + offset %0d",pc,$signed(offset_w));
//                     2'b10://jmpc
//                         $display("JMPC PC 0x%0x + offset %0d",pc,$signed(offset_w));
//                     2'b11://jmpnc
//                         $display("JMPNC PC 0x%0x + offset %0d",pc,$signed(offset_w));
//                 endcase
//
//                 //Arithmetic
//                 risc_pack::ALU:
//                 case(instruction[27:25])
//                     3'd0:$display("AND R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     3'd1:$display("OR  R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     3'd2:$display("XOR R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     3'd3:$display("ADD R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     3'd4:$display("MUL R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     3'd5:$display("SHL R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     3'd6:$display("SHR R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     default:$display("CMP R%0d with R%0d+%d",id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                 endcase
//
//                 risc_pack::LDRF:
//                 case(instruction[27:26])
//                     2'b00://ldrz
//                         $display("LDRZ R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     2'b01://ldrnz
//                         $display("LDRNZ R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     2'b10://ldrc
//                         $display("LDRC R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                     2'b11://ldrnc
//                         $display("LDRNC R%0d,R%0d,R%0d+%d",id_out_w.dst_r,id_out_w.src_r1,id_out_w.src_r2,$signed(id_out_w.incr_r2));
//                 endcase
//
//                 default:
//                     if(!reset)
//                     begin
//                         $display("Unknown Command %x",instruction[31:28]);
//                     end
//             endcase
//             end
//             else
//                 $display("[ID stage] STALLED!",instruction[31:28]);
//         end
// //synthesys translate_on
//     end
endmodule
