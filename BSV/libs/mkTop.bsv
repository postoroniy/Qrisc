import Vector::*;
import StmtFSM::*;
import mkMultiCycleFunction::*;

typedef 4 VecSize;
typedef 3 MCFcycles;

typedef 10000 TestCycles;
typedef TLog#(TestCycles) CycleBits;

function Bit#(32) sum_fn(Vector#(VecSize, Bit#(32))  vec);
    return foldr (\+ , 0, vec);
endfunction

module mkTop(Empty);
    let m <- mkMultiCycleFunction(valueOf(MCFcycles),sum_fn);

    Vector#(VecSize, Reg#(Bit#(32))) test_input <- replicateM(mkRegU);
    Reg#(Bit#(32)) expect_result <- mkRegU;
    Reg#(Bit#(CycleBits)) cycle <- mkRegU;

    Stmt test = seq
        noAction;
        for(cycle<=0;cycle < fromInteger(valueOf(TestCycles));cycle <= cycle+1)seq
            action
                $display("Cycle %0d", cycle);
                for (Integer i = 0; i < valueOf(VecSize); i = i + 1)begin
                    let r <- $random;
                    test_input[i] <= pack(r);
                    $display("random %0x", pack(r));
                end
            endaction
            par
                m.start(readVReg(test_input));
                expect_result <= sum_fn(readVReg(test_input));
            endpar
            // while (!m.done) noAction; //can be removed as Method in MCF is blocking
            if (expect_result == m.result)
                $display("\033[32mTest passed\033[0m");
            else action
                $display("\033[31mTest failed\033[0m, expected = %0x, got = %0x", expect_result, m.result);
                $fatal;
            endaction
        endseq
        $finish;
    endseq;

    mkAutoFSM(test);
endmodule
