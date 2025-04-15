import Vector::*;

interface FunctionModuleIfc#(numeric type n, type t_in, type t_out);
    method Action start(Vector#(n, t_in) inputs);
    method Bool done;
    method t_out result;
endinterface

module [Module] mkMultiCycleFunction#(
    Integer n_cycles,
    function t_out f_fn(Vector#(n, t_in) inputs)
)(
    FunctionModuleIfc#(n, t_in, t_out)
)
    provisos (
        Bits#(t_in, _),
        Bits#(t_out, _),
        Literal#(t_out)
    );

    Reg#(UInt#(8)) counter <- mkReg(0);
    Reg#(Vector#(n, t_in)) inputs_reg <- mkRegU;

    Reg#(Bool) active <- mkReg(False);
    Reg#(t_out) result_reg <- mkRegU;

    rule run_multi_cycle (counter != 0);
        counter <= counter - 1;
        result_reg <= f_fn(inputs_reg);
    endrule

    rule clear_done (counter == 0 && active);
        active <= False;
    endrule

    method Action start(Vector#(n, t_in) inputs) if (!active);
        inputs_reg <= inputs;
        counter <= fromInteger(n_cycles - 1);
        active <= True;
    endmethod

    method Bool done = (active && counter == 0);

    method t_out result if (active && counter == 0);
        //return 0; for testing
        return result_reg;
    endmethod
endmodule
