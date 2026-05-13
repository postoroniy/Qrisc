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
`default_nettype none

module Qrisc32ID(
        input logic         clk,areset,
        input logic[31:0]   instruction,
        input logic[31:0]   pc,

        input logic         pipe_stall,//feed back from MEM stage
        input Qrisc_pack::pipe_struct_t   pipe_fwd_mem,//for memory read forwarding
        input Qrisc_pack::pipe_struct_t   pipe_fwd_ex,//for EX forwarding
        input Qrisc_pack::pipe_struct_t   pipe_wb_mem,//registered MEM writeback forwarding
        input Qrisc_pack::pipe_struct_t   pipe_wb_ex,//registered EX writeback forwarding
        input logic[31:0]   rf_val_r1,
        input logic[31:0]   rf_val_r2,
        input logic[31:0]   rf_val_dst,

        output Qrisc_pack::pipe_struct_t  pipe_id_out,
        input logic               verbose
    );

    Qrisc_pack::pipe_struct_t     pipe_id_out_w;
    logic[31:0]       offset_w;
    logic[31:0]       nop_counter;
    logic[31:0]       jmp_counter;
    logic[31:0]       alu_counter;
    logic[31:0]       oth_counter;

    function automatic logic[31:0] forwarded_value(
        input logic[4:0] reg_idx,
        input logic[31:0] rf_value
    );
        begin
            if(pipe_fwd_mem.write_reg && pipe_fwd_mem.dst_r==reg_idx)
                forwarded_value = pipe_fwd_mem.val_dst;
            else if(pipe_fwd_ex.write_reg && pipe_fwd_ex.dst_r==reg_idx)
                forwarded_value = pipe_fwd_ex.val_dst;
            else if(pipe_fwd_ex.incr_r2_enable && pipe_fwd_ex.src_r2==reg_idx)
                forwarded_value = pipe_fwd_ex.val_r2;
            else if(pipe_wb_mem.write_reg && pipe_wb_mem.dst_r==reg_idx)
                forwarded_value = pipe_wb_mem.val_dst;
            else if(pipe_wb_ex.write_reg && pipe_wb_ex.dst_r==reg_idx)
                forwarded_value = pipe_wb_ex.val_dst;
            else if(pipe_wb_ex.incr_r2_enable && pipe_wb_ex.src_r2==reg_idx)
                forwarded_value = pipe_wb_ex.val_r2;
            else
                forwarded_value = rf_value;
        end
    endfunction

    //comb part
    always_comb begin
        pipe_id_out_w.dst_r     = instruction[04:00];
        pipe_id_out_w.src_r1    = instruction[09:05];
        pipe_id_out_w.src_r2    = instruction[14:10];
        pipe_id_out_w.incr_r2   = 0;
        pipe_id_out_w.incr_r2_enable    = 1;

        pipe_id_out_w.read_mem  = 0;
        pipe_id_out_w.write_mem = 0;
        pipe_id_out_w.write_reg = 0;

        pipe_id_out_w.mul_op    = 0;
        pipe_id_out_w.add_op    = 0;
        pipe_id_out_w.or_op     = 0;
        pipe_id_out_w.and_op    = 0;
        pipe_id_out_w.xor_op    = 0;
        pipe_id_out_w.shl_op    = 0;
        pipe_id_out_w.shr_op    = 0;
        pipe_id_out_w.cmp_op    = 0;
        pipe_id_out_w.ldrf_op   = 0;

        pipe_id_out_w.jmpunc    = 0;
        pipe_id_out_w.jmpz      = 0;
        pipe_id_out_w.jmpnz     = 0;
        pipe_id_out_w.jmpc      = 0;
        pipe_id_out_w.jmpnc     = 0;

        pipe_id_out_w.val_r1    = forwarded_value(pipe_id_out_w.src_r1, rf_val_r1);
        pipe_id_out_w.val_r2    = forwarded_value(pipe_id_out_w.src_r2, rf_val_r2);
        pipe_id_out_w.val_dst   = forwarded_value(pipe_id_out_w.dst_r,  rf_val_dst);

        offset_w = (instruction[25])?pipe_id_out_w.val_r2:{{17{instruction[24]}},instruction[24:10]};//17 bit sign + 15 bit offset

        unique case(instruction[24:22])
            0:begin pipe_id_out_w.incr_r2=4'd0;pipe_id_out_w.incr_r2_enable= 0;end
            1:pipe_id_out_w.incr_r2=4'd1;
            2:pipe_id_out_w.incr_r2=4'd2;
            3:pipe_id_out_w.incr_r2=4'd4;
            4:begin pipe_id_out_w.incr_r2=4'd0;pipe_id_out_w.incr_r2_enable= 0;end
            5:pipe_id_out_w.incr_r2=-4'd1;
            6:pipe_id_out_w.incr_r2=-4'd2;
            7:pipe_id_out_w.incr_r2=-4'd4;
        endcase

        case(instruction[31:28])
            //load and store
            Qrisc_pack::LDR:
            unique case(instruction[27:26])
                2'b00: begin
                        pipe_id_out_w.write_reg = (pipe_id_out_w.dst_r!=pipe_id_out_w.src_r1)?1:0;//write from reg src1 to reg dst
                        //pipe_id_out_w.incr_r2_enable    = pipe_id_out_w.write_reg;
                        pipe_id_out_w.val_dst = pipe_id_out_w.val_r1;
                    end
                2'b01:begin
                        pipe_id_out_w.val_dst[31:16]= instruction[20:5];
                        pipe_id_out_w.write_reg     = 1;//write from reg src1 to reg dst
                        pipe_id_out_w.incr_r2_enable= 0;
                    end
                2'b10:begin
                        pipe_id_out_w.val_dst[15:0] = instruction[20:5];
                        pipe_id_out_w.write_reg     = 1;//write from reg src1 to reg dst
                        pipe_id_out_w.incr_r2_enable= 0;
                    end
                2'b11:begin
                        pipe_id_out_w.read_mem      = 1;//read from mem by Rscr2+Rsrc1 and then write to register
                        pipe_id_out_w.write_reg     = 1;//write from reg src1 to reg dst
                        pipe_id_out_w.incr_r2_enable= instruction[25];
                        pipe_id_out_w.val_r2        = offset_w;
                    end
            endcase

            Qrisc_pack::STR:
            case(instruction[27:26])
                //2'b11:
                default: begin
                    pipe_id_out_w.write_mem     = 1;//write Rdst to mem by Rscr2+Rsrc1
                    pipe_id_out_w.val_r2        = offset_w;
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                end
            endcase

            //jumps
            Qrisc_pack::JMPUNC:
            unique case(instruction[27:26])
                2'b00: begin
                    pipe_id_out_w.val_r1        = {6'd0, instruction[25:0]};
                    pipe_id_out_w.val_r2        = '0;//no offset
                    pipe_id_out_w.incr_r2_enable= 0;
                    pipe_id_out_w.jmpunc        = 1;
                end
                2'b01: begin
                //relative jump
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpunc        = 1;
                end
                2'b10:begin
                    //call
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.val_dst       = pc;//return address
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpunc        = 1;
                    pipe_id_out_w.write_reg     = 1;
                end
                2'b11:begin
                //ret
                    pipe_id_out_w.val_r1        = pipe_id_out_w.val_dst;
                    pipe_id_out_w.val_r2        = '0;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpunc        = 1;
                end
            endcase

            Qrisc_pack::JMPF:
            unique case(instruction[27:26])
                2'b00:begin
                //jmpz
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpz          = 1;
                end
                2'b01:begin
                //jmpnz
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpnz         = 1;
                end
                2'b10:begin
                //jmpc
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpc          = 1;
                end
                2'b11:begin
                //jmpnc
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpnc         = 1;
                end
            endcase

            //Arithmetic
            Qrisc_pack::ALU:
            unique case(instruction[27:25])
                3'd0: begin
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.and_op    = 1;
                end
                3'd1:begin
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.or_op     = 1;
                end
                3'd2:begin
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.xor_op    = 1;
                end
                3'd3:begin
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.add_op    = 1;
                end
                3'd4:begin
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.mul_op    = 1;
                end
                3'd5:begin
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.shl_op    = 1;
                end
                3'd6:begin
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.shr_op    = 1;
                end
                3'd7:begin
                    //pipe_id_out_w.write_reg    = 0;
                    pipe_id_out_w.cmp_op    = 1;
                end
            endcase

            Qrisc_pack::LDRF:
            unique case(instruction[27:26])
                2'b00:begin
                //ldrz
                    pipe_id_out_w.jmpz      = 1;
                    pipe_id_out_w.ldrf_op   = 1;
                    pipe_id_out_w.write_reg = 1;
                end
                2'b01:begin
                //ldrnz
                    pipe_id_out_w.jmpnz     = 1;
                    pipe_id_out_w.ldrf_op   = 1;
                    pipe_id_out_w.write_reg = 1;
                end
                2'b10:begin
                //ldrc
                    pipe_id_out_w.ldrf_op   = 1;
                    pipe_id_out_w.jmpc      = 1;
                    pipe_id_out_w.write_reg = 1;
                end
                2'b11:begin
                //ldrnc
                    pipe_id_out_w.ldrf_op   = 1;
                    pipe_id_out_w.jmpnc     = 1;
                    pipe_id_out_w.write_reg = 1;
                end
            endcase

            default:begin
                    pipe_id_out_w='0;
                end
        endcase
    end

    always_ff@(posedge clk or posedge areset)
    if(areset)begin
        pipe_id_out<='0;
        nop_counter<='0;
        jmp_counter<='0;
        alu_counter<='0;
        oth_counter<='0;
    end else begin
        if(instruction==0)
            nop_counter<=nop_counter+1;
        else if(instruction[31:28]==Qrisc_pack::JMPUNC || instruction[31:28]==Qrisc_pack::JMPF)
            jmp_counter<=jmp_counter+1;
        else if(instruction[31:28]==Qrisc_pack::ALU)
            alu_counter<=alu_counter+1;
        else
            oth_counter<=oth_counter+1;

        if(~pipe_stall)
            pipe_id_out<=pipe_id_out_w;

//synthesys translate_off
        if(verbose) begin
            if(~pipe_stall) begin
                case(instruction[31:28])
                    //load and store
                    Qrisc_pack::LDR:
                    unique case(instruction[27:26])
                        2'b00:$display("LDR R%0d, R%0d, R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        2'b01:$display("LDRH R%0d,0x%x",pipe_id_out_w.dst_r,instruction[20:5]);
                        2'b10:$display("LDRL R%0d,0x%x",pipe_id_out_w.dst_r,instruction[20:5]);
                        2'b11:$display("LDRP R%0d,[R%0d +%0d],R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,$signed(offset_w),pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                    endcase

                    Qrisc_pack::STR:
                    case(instruction[27:26])
                        default:$display("STR R%0d,[R%0d +%0d],R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,$signed(offset_w),pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                    endcase

                    //jumps
                    Qrisc_pack::JMPUNC:
                    unique case(instruction[27:26])
                        2'b00:$display("JMP 0x%0x",instruction[25:0]);
                        2'b01://relative jump
                        $display("JMPR PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        2'b10://call
                        $display("CALLR PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        2'b11://ret
                        $display("RET to 0x%0x",pipe_id_out_w.val_dst);
                    endcase

                    Qrisc_pack::JMPF:
                    unique case(instruction[27:26])
                        2'b00://jmpz
                            $display("JMPZ PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        2'b01://jmpnz
                            $display("JMPNZ PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        2'b10://jmpc
                            $display("JMPC PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        2'b11://jmpnc
                            $display("JMPNC PC 0x%0x + offset %0d",pc,$signed(offset_w));
                    endcase

                    //Arithmetic
                    Qrisc_pack::ALU:
                    unique case(instruction[27:25])
                        3'd0:$display("AND R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        3'd1:$display("OR  R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        3'd2:$display("XOR R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        3'd3:$display("ADD R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        3'd4:$display("MUL R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        3'd5:$display("SHL R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        3'd6:$display("SHR R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        3'd7:$display("CMP R%0d with R%0d+%d",pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                    endcase

                    Qrisc_pack::LDRF:
                    unique case(instruction[27:26])
                        2'b00://ldrz
                            $display("LDRZ R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        2'b01://ldrnz
                            $display("LDRNZ R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        2'b10://ldrc
                            $display("LDRC R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        2'b11://ldrnc
                            $display("LDRNC R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                    endcase

                    default:
                        if(!areset)
                        begin
                            $display("Unknown Command %x",instruction[31:28]);
                        end
                endcase
            end else
                $display("[ID stage] STALLED!",instruction[31:28]);
        end
//synthesys translate_on
    end
endmodule

`default_nettype wire
