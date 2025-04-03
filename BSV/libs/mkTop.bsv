import Vector::*;
import FIFO::*;
import StmtFSM::*;

import mkMultiCycleFunction::*;

function Bit#(32) sum_fn(Vector#(4, Bit#(32)) vec);
    return foldr (\+ , 0, vec);
endfunction

module mkTop(Empty);
    let m <- mkMultiCycleFunction(7,sum_fn);
    Vector#(4, Bit#(32)) test_input = replicate(42);
    Bit#(32) expect_result = sum_fn(test_input);

    Stmt test = seq
        $display("Start computation with input = %0d %0d %0d %0d",
                 test_input[0], test_input[1], test_input[2], test_input[3]);
        $display("Expected = %0d", expect_result);
        m.start(test_input);
        while (!m.done) noAction;
        $display("Test %s",
                 expect_result==m.result ?
                 "passed" :
                 "failed");
    endseq;

    mkAutoFSM(test);
endmodule
