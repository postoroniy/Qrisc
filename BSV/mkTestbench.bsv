import Qrisc_pack::*;
import AXI4_Types :: *;
import RegFile :: * ;
import FIFOF :: *;
import Vector :: *;
import Semi_FIFOF::*;
import qrisc32 :: *;

// import GetPut       :: *;
// import ClientServer :: *;
import Connectable  :: *;

module mkTestbench(Empty);

    // Clock and Reset
    Reg#(Bit#(32)) cycle_count <- mkReg(0);

    // Memory Emulation with initialization
    RegFile#(Bit#(32), Bit#(32)) memory <- mkRegFileLoad("mem_init.hex", 0, 1023);

    // Instantiate AXI4 Slave Transactors
    AXI4_Slave_Xactor_IFC#(1,32,32,1) axi_inst <- mkAXI4_Slave_Xactor_2;
    AXI4_Slave_Xactor_IFC#(1,32,32,1) axi_data <- mkAXI4_Slave_Xactor_2;

    // Connect AXI interfaces to CPU interface
    Qrisc_if#(1,32,32,1) cpu <- qrisc32;
    mkConnection(cpu.axi_instruction, axi_inst.axi_side);
    mkConnection(cpu.axi_data, axi_data.axi_side);

   // Instruction fetch handling
    rule inst_fetch;
        let req <- axi_inst.master_side.getReq;
        Bit#(32) addr = truncate(req.addr);
        Bit#(32) data = memory.sub(addr >> 2);
        axi_inst.master_side.putResp(AXI4_ReadResp {id: req.id, data: data, resp: OKAY, user: 0});
    endrule

    // // Data read handling
    // rule data_read;
    //     let req <- axi_data.master_side.getReq;
    //     Bit#(32) addr = truncate(req.addr);
    //     Bit#(32) data = memory.sub(addr >> 2);
    //     axi_data.master_side.putResp(AXI4_ReadResp {id: req.id, data: data, resp: OKAY, user: 0});
    // endrule

    // // Data write handling
    // rule data_write;
    //     let req <- axi_data.master_side.getWrite;
    //     Bit#(32) addr = truncate(req.addr);
    //     memory.upd(addr >> 2, req.data);
    //     axi_data.master_side.putWriteResp(AXI4_WriteResp {id: req.id, resp: OKAY, user: 0});
    // endrule

    // rule simulate_cycle;
    //     cycle_count <= cycle_count + 1;

    //     $display("Cycle %d:", cycle_count);
    //     $display("PC: %h", cpu.getPC);
    //     $display("Instruction: %h", cpu.getInst);
    // endrule

    // rule terminate_simulation (cycle_count == 1000);
    //     $display("Simulation completed at cycle %d.", cycle_count);
    //     $finish;
    // endrule
endmodule