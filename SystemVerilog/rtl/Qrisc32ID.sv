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

        input logic         mem_stall,//feed back from MEM stage
        input logic         branch_flush,
        output logic        hazard_stall,
        input Qrisc_pack::pipe_struct_t   pipe_fwd_mem,//for memory read forwarding
        input Qrisc_pack::pipe_struct_t   pipe_fwd_ex,//for EX forwarding
        input Qrisc_pack::pipe_struct_t   pipe_wb_mem,//registered MEM writeback forwarding
        input Qrisc_pack::pipe_struct_t   pipe_wb_ex,//registered EX writeback forwarding
        input logic         mem_load_pending,
        input logic[4:0]    mem_load_dst,
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
    Qrisc_pack::opcode_t      opcode;
    Qrisc_pack::load_mode_t   load_mode;
    Qrisc_pack::jump_mode_t   jump_mode;
    Qrisc_pack::flag_mode_t   flag_mode;
    Qrisc_pack::alu_op_t      alu_op;
    Qrisc_pack::incr_mode_t   incr_mode;
    logic use_src_r1;
    logic use_src_r2;
    logic use_dst_r;
    logic raw_hazard_stall;
    logic load_hazard_stall;

    function automatic logic[31:0] forwarded_value(
        input logic[4:0] reg_idx,
        input logic[31:0] rf_value
    );
        begin
            if(pipe_fwd_ex.write_reg && !pipe_fwd_ex.read_mem && pipe_fwd_ex.dst_r==reg_idx)
                forwarded_value = pipe_fwd_ex.val_dst;
            else if(pipe_fwd_ex.incr_r2_enable && pipe_fwd_ex.src_r2==reg_idx)
                forwarded_value = pipe_fwd_ex.val_r2;
            else if(pipe_fwd_mem.write_reg && pipe_fwd_mem.dst_r==reg_idx)
                forwarded_value = pipe_fwd_mem.val_dst;
            else if(pipe_fwd_mem.incr_r2_enable && pipe_fwd_mem.src_r2==reg_idx)
                forwarded_value = pipe_fwd_mem.val_r2;
            else if(pipe_wb_ex.write_reg && pipe_wb_ex.dst_r==reg_idx)
                forwarded_value = pipe_wb_ex.val_dst;
            else if(pipe_wb_ex.incr_r2_enable && pipe_wb_ex.src_r2==reg_idx)
                forwarded_value = pipe_wb_ex.val_r2;
            else if(pipe_wb_mem.write_reg && pipe_wb_mem.dst_r==reg_idx)
                forwarded_value = pipe_wb_mem.val_dst;
            else if(pipe_wb_mem.incr_r2_enable && pipe_wb_mem.src_r2==reg_idx)
                forwarded_value = pipe_wb_mem.val_r2;
            else
                forwarded_value = rf_value;
        end
    endfunction

    function automatic logic reads_reg(
        input logic[4:0] reg_idx,
        input logic use1,
        input logic[4:0] reg1,
        input logic use2,
        input logic[4:0] reg2,
        input logic used,
        input logic[4:0] regd
    );
        begin
            reads_reg = (use1 && reg1 == reg_idx) ||
                        (use2 && reg2 == reg_idx) ||
                        (used && regd == reg_idx);
        end
    endfunction

    function automatic logic pipe_reg_hazard(
        input logic producer_write_reg,
        input logic[4:0] producer_dst_r,
        input logic producer_incr_r2_enable,
        input logic[4:0] producer_src_r2,
        input logic use1,
        input logic[4:0] reg1,
        input logic use2,
        input logic[4:0] reg2,
        input logic used,
        input logic[4:0] regd
    );
        begin
            pipe_reg_hazard =
                (producer_write_reg && reads_reg(producer_dst_r, use1, reg1, use2, reg2, used, regd)) ||
                (producer_incr_r2_enable && reads_reg(producer_src_r2, use1, reg1, use2, reg2, used, regd));
        end
    endfunction

    //comb part
    always_comb begin
        opcode    = Qrisc_pack::opcode_t'(instruction[31:28]);
        load_mode = Qrisc_pack::load_mode_t'(instruction[27:26]);
        jump_mode = Qrisc_pack::jump_mode_t'(instruction[27:26]);
        flag_mode = Qrisc_pack::flag_mode_t'(instruction[27:26]);
        alu_op    = Qrisc_pack::alu_op_t'(instruction[27:25]);
        incr_mode = Qrisc_pack::incr_mode_t'(instruction[24:22]);

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
        use_src_r1 = 1'b0;
        use_src_r2 = 1'b0;
        use_dst_r  = 1'b0;

        offset_w = (instruction[25])?pipe_id_out_w.val_r2:{{17{instruction[24]}},instruction[24:10]};//17 bit sign + 15 bit offset

        unique case(incr_mode)
            Qrisc_pack::INCR_NONE:begin pipe_id_out_w.incr_r2=4'd0;pipe_id_out_w.incr_r2_enable= 0;end
            Qrisc_pack::INCR_ONE:pipe_id_out_w.incr_r2=4'd1;
            Qrisc_pack::INCR_TWO:pipe_id_out_w.incr_r2=4'd2;
            Qrisc_pack::INCR_FOUR:pipe_id_out_w.incr_r2=4'd4;
            Qrisc_pack::INCR_NONE_ALT:begin pipe_id_out_w.incr_r2=4'd0;pipe_id_out_w.incr_r2_enable= 0;end
            Qrisc_pack::DECR_ONE:pipe_id_out_w.incr_r2=-4'd1;
            Qrisc_pack::DECR_TWO:pipe_id_out_w.incr_r2=-4'd2;
            Qrisc_pack::DECR_FOUR:pipe_id_out_w.incr_r2=-4'd4;
        endcase

        case(opcode)
            //load and store
            Qrisc_pack::LDR:
            unique case(load_mode)
                Qrisc_pack::LOAD_REG: begin
                        use_src_r1 = 1'b1;
                        use_src_r2 = pipe_id_out_w.incr_r2_enable;
                        pipe_id_out_w.write_reg = (pipe_id_out_w.dst_r!=pipe_id_out_w.src_r1)?1:0;//write from reg src1 to reg dst
                        //pipe_id_out_w.incr_r2_enable    = pipe_id_out_w.write_reg;
                        pipe_id_out_w.val_dst = pipe_id_out_w.val_r1;
                    end
                Qrisc_pack::LOAD_HIGH:begin
                        use_dst_r = 1'b1;
                        pipe_id_out_w.val_dst[31:16]= instruction[20:5];
                        pipe_id_out_w.write_reg     = 1;//write from reg src1 to reg dst
                        pipe_id_out_w.incr_r2_enable= 0;
                    end
                Qrisc_pack::LOAD_LOW:begin
                        use_dst_r = 1'b1;
                        pipe_id_out_w.val_dst[15:0] = instruction[20:5];
                        pipe_id_out_w.write_reg     = 1;//write from reg src1 to reg dst
                        pipe_id_out_w.incr_r2_enable= 0;
                    end
                Qrisc_pack::LOAD_MEM:begin
                        use_src_r1 = 1'b1;
                        use_src_r2 = instruction[25];
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
                    use_src_r1 = 1'b1;
                    use_src_r2 = instruction[25];
                    use_dst_r  = 1'b1;
                    pipe_id_out_w.write_mem     = 1;//write Rdst to mem by Rscr2+Rsrc1
                    pipe_id_out_w.val_r2        = offset_w;
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                end
            endcase

            //jumps
            Qrisc_pack::JMPUNC:
            unique case(jump_mode)
                Qrisc_pack::JUMP_ABS: begin
                    pipe_id_out_w.val_r1        = {6'd0, instruction[25:0]};
                    pipe_id_out_w.val_r2        = '0;//no offset
                    pipe_id_out_w.incr_r2_enable= 0;
                    pipe_id_out_w.jmpunc        = 1;
                end
                Qrisc_pack::JUMP_REL: begin
                //relative jump
                    use_src_r2 = instruction[25];
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpunc        = 1;
                end
                Qrisc_pack::JUMP_CALL:begin
                    //call
                    use_src_r2 = instruction[25];
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.val_dst       = pc;//return address
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpunc        = 1;
                    pipe_id_out_w.write_reg     = 1;
                end
                Qrisc_pack::JUMP_RET:begin
                //ret
                    use_dst_r  = 1'b1;
                    use_src_r2 = instruction[25];
                    pipe_id_out_w.val_r1        = pipe_id_out_w.val_dst;
                    pipe_id_out_w.val_r2        = '0;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpunc        = 1;
                end
            endcase

            Qrisc_pack::JMPF:
            unique case(flag_mode)
                Qrisc_pack::FLAG_Z:begin
                //jmpz
                    use_src_r2                 = instruction[25];
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpz          = 1;
                end
                Qrisc_pack::FLAG_NZ:begin
                //jmpnz
                    use_src_r2                 = instruction[25];
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpnz         = 1;
                end
                Qrisc_pack::FLAG_C:begin
                //jmpc
                    use_src_r2                 = instruction[25];
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpc          = 1;
                end
                Qrisc_pack::FLAG_NC:begin
                //jmpnc
                    use_src_r2                 = instruction[25];
                    pipe_id_out_w.val_r1        = pc;
                    pipe_id_out_w.val_r2        = offset_w;//offset
                    pipe_id_out_w.incr_r2_enable= instruction[25];
                    pipe_id_out_w.jmpnc         = 1;
                end
            endcase

            //Arithmetic
            Qrisc_pack::ALU:
            unique case(alu_op)
                Qrisc_pack::ALU_AND: begin
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.and_op    = 1;
                end
                Qrisc_pack::ALU_OR:begin
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.or_op     = 1;
                end
                Qrisc_pack::ALU_XOR:begin
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.xor_op    = 1;
                end
                Qrisc_pack::ALU_ADD:begin
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.add_op    = 1;
                end
                Qrisc_pack::ALU_MUL:begin
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.mul_op    = 1;
                end
                Qrisc_pack::ALU_SHL:begin
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.shl_op    = 1;
                end
                Qrisc_pack::ALU_SHR:begin
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.write_reg = 1;
                    pipe_id_out_w.shr_op    = 1;
                end
                Qrisc_pack::ALU_CMP:begin
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    //pipe_id_out_w.write_reg    = 0;
                    pipe_id_out_w.cmp_op    = 1;
                end
            endcase

            Qrisc_pack::LDRF:
            unique case(flag_mode)
                Qrisc_pack::FLAG_Z:begin
                //ldrz
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.jmpz      = 1;
                    pipe_id_out_w.ldrf_op   = 1;
                    pipe_id_out_w.write_reg = 1;
                end
                Qrisc_pack::FLAG_NZ:begin
                //ldrnz
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.jmpnz     = 1;
                    pipe_id_out_w.ldrf_op   = 1;
                    pipe_id_out_w.write_reg = 1;
                end
                Qrisc_pack::FLAG_C:begin
                //ldrc
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.ldrf_op   = 1;
                    pipe_id_out_w.jmpc      = 1;
                    pipe_id_out_w.write_reg = 1;
                end
                Qrisc_pack::FLAG_NC:begin
                //ldrnc
                    use_src_r1 = 1'b1;
                    use_src_r2 = 1'b1;
                    pipe_id_out_w.ldrf_op   = 1;
                    pipe_id_out_w.jmpnc     = 1;
                    pipe_id_out_w.write_reg = 1;
                end
            endcase

            default:begin
                    pipe_id_out_w='0;
                end
        endcase

        raw_hazard_stall = pipe_reg_hazard(
            pipe_id_out.write_reg,
            pipe_id_out.dst_r,
            pipe_id_out.incr_r2_enable,
            pipe_id_out.src_r2,
            use_src_r1,
            pipe_id_out_w.src_r1,
            use_src_r2,
            pipe_id_out_w.src_r2,
            use_dst_r,
            pipe_id_out_w.dst_r
        );
        load_hazard_stall = mem_load_pending && reads_reg(
            mem_load_dst,
            use_src_r1,
            pipe_id_out_w.src_r1,
            use_src_r2,
            pipe_id_out_w.src_r2,
            use_dst_r,
            pipe_id_out_w.dst_r
        );
        hazard_stall = !branch_flush && (raw_hazard_stall || load_hazard_stall);
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
        else if(opcode==Qrisc_pack::JMPUNC || opcode==Qrisc_pack::JMPF)
            jmp_counter<=jmp_counter+1;
        else if(opcode==Qrisc_pack::ALU)
            alu_counter<=alu_counter+1;
        else
            oth_counter<=oth_counter+1;

        if(mem_stall) begin
            pipe_id_out <= pipe_id_out;
        end else if(branch_flush || hazard_stall) begin
            pipe_id_out <= '0;
        end else begin
            pipe_id_out<=pipe_id_out_w;
        end

// synthesis translate_off
        if(verbose) begin
            if(~mem_stall && ~hazard_stall && ~branch_flush) begin
                case(opcode)
                    //load and store
                    Qrisc_pack::LDR:
                    unique case(load_mode)
                        Qrisc_pack::LOAD_REG:$display("LDR R%0d, R%0d, R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::LOAD_HIGH:$display("LDRH R%0d,0x%x",pipe_id_out_w.dst_r,instruction[20:5]);
                        Qrisc_pack::LOAD_LOW:$display("LDRL R%0d,0x%x",pipe_id_out_w.dst_r,instruction[20:5]);
                        Qrisc_pack::LOAD_MEM:$display("LDRP R%0d,[R%0d +%0d],R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,$signed(offset_w),pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                    endcase

                    Qrisc_pack::STR:
                    case(load_mode)
                        default:$display("STR R%0d,[R%0d +%0d],R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,$signed(offset_w),pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                    endcase

                    //jumps
                    Qrisc_pack::JMPUNC:
                    unique case(jump_mode)
                        Qrisc_pack::JUMP_ABS:$display("JMP 0x%0x",instruction[25:0]);
                        Qrisc_pack::JUMP_REL://relative jump
                        $display("JMPR PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        Qrisc_pack::JUMP_CALL://call
                        $display("CALLR PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        Qrisc_pack::JUMP_RET://ret
                        $display("RET to 0x%0x",pipe_id_out_w.val_dst);
                    endcase

                    Qrisc_pack::JMPF:
                    unique case(flag_mode)
                        Qrisc_pack::FLAG_Z://jmpz
                            $display("JMPZ PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        Qrisc_pack::FLAG_NZ://jmpnz
                            $display("JMPNZ PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        Qrisc_pack::FLAG_C://jmpc
                            $display("JMPC PC 0x%0x + offset %0d",pc,$signed(offset_w));
                        Qrisc_pack::FLAG_NC://jmpnc
                            $display("JMPNC PC 0x%0x + offset %0d",pc,$signed(offset_w));
                    endcase

                    //Arithmetic
                    Qrisc_pack::ALU:
                    unique case(alu_op)
                        Qrisc_pack::ALU_AND:$display("AND R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::ALU_OR:$display("OR  R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::ALU_XOR:$display("XOR R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::ALU_ADD:$display("ADD R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::ALU_MUL:$display("MUL R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::ALU_SHL:$display("SHL R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::ALU_SHR:$display("SHR R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::ALU_CMP:$display("CMP R%0d with R%0d+%d",pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                    endcase

                    Qrisc_pack::LDRF:
                    unique case(flag_mode)
                        Qrisc_pack::FLAG_Z://ldrz
                            $display("LDRZ R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::FLAG_NZ://ldrnz
                            $display("LDRNZ R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::FLAG_C://ldrc
                            $display("LDRC R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                        Qrisc_pack::FLAG_NC://ldrnc
                            $display("LDRNC R%0d,R%0d,R%0d+%d",pipe_id_out_w.dst_r,pipe_id_out_w.src_r1,pipe_id_out_w.src_r2,$signed(pipe_id_out_w.incr_r2));
                    endcase

                    default:
                        if(!areset)
                        begin
                            $display("Unknown Command %x",instruction[31:28]);
                        end
                endcase
            end else if(mem_stall)
                $display("[ID stage] STALLED!",instruction[31:28]);
            else if(hazard_stall)
                $display("[ID stage] HAZARD BUBBLE!",instruction[31:28]);
            else
                $display("[ID stage] BRANCH FLUSH!",instruction[31:28]);
        end
// synthesis translate_on
    end
endmodule

`default_nettype wire
