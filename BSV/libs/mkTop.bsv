import Vector::*;
import StmtFSM::*;
import mkMultiCycleFunction::*;

// import "BDPI" function ActionValue#(Bit#(32)) get_time();
// import "BDPI" function Action seed_rng(Bit#(32) seed);
// import "BDPI" function ActionValue#(Bit#(32)) urandom();

function Bit#(32) sum_fn(Vector#(4, Bit#(32))  vec);
    return foldr (\+ , 0, vec);
endfunction

module mkTop(Empty);
    let m <- mkMultiCycleFunction(7,sum_fn);

    Vector#(4, Reg#(Bit#(32))) test_input <- replicateM(mkRegU);
    Reg#(Bit#(32)) expect_result <- mkRegU;
    Reg#(Bit#(32)) result <- mkRegU;

    // Reg#(Bit#(62)) seed <- mkReg(2011111);
    // Reg#(Int#(32)) rnd <- mkReg(12314);
    // rule test;
    //     $display("Seq00");
    //     $display("Seed = %0d", seed);
    //     let r <- $random();
    //     rnd <= r;
    //     $display("Random = %0d", r);
    //     let ra <- $urandom_range(111,Just(100));
    //     $display("Random range= %0d", ra);
    //     let rb <- $urandom_range(101,Nothing);
    //     $display("Random range= %0d", rb);
    //     let seeded <- $test$plusargs("seed");
    //     if (seeded)
    //         $display("Test pluargs!");
    //     $display("Seq01");
    //     let bseed <- $value$plusargs("seed=%d",seed);
    //     if (bseed)
    //         $display("Seed = %0d", seed);
    //     else
    //         $display("Seed not valid");
    //     $display("Seq02");
    //     $finish;
    //      let r <- urandom();
    //      $display("Random = %0d", r);
    //      let t <- get_time();
    //     $display("Time = %0d", t);
    //     seed_rng(t);
    //     let r <- urandom();
    //     $display("Random = %0d", r);
    // endrule
    Stmt test = seq
        seq
            action
                Vector#(4, Bit#(32)) values = replicate(0);
                for (Integer i = 0; i < 4; i = i + 1)begin
                    let r <- $random;
                    test_input[i] <= pack(r);
                    values[i] = test_input[i];
                end
                expect_result <= sum_fn(values);
                m.start(values);
            endaction
        endseq
        while (!m.done) noAction;
        result <= m.result;

        $display("Test %s",
                 expect_result==result ?
                 "passed" :
                 "failed");

        $display("expected result = %0d\nresult = %0d", expect_result,result);
        $finish;
    endseq;

    mkAutoFSM(test);
endmodule
