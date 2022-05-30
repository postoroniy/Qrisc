#include "Vqrisc32_tb.h"
#include <memory.h>
#include <verilated.h>
#if VM_TRACE
#include "verilated_fst_c.h"
#endif

int main(int argc, char **argv, char **env) {
  VerilatedContext *contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);
  Vqrisc32_tb *top = new Vqrisc32_tb{contextp};

#if VM_TRACE
  Verilated::traceEverOn(true);
  VerilatedFstC *tfp = new VerilatedFstC;
  top->trace(tfp, 99); // Trace 99 levels of hierarchy
  tfp->open("qrisc32_tb.vcd");
#endif
  top->reset = 1;        // Assert reset
  top->verbose = 0;      //
  top->Istop_enable = 0; //
  top->Dstop_enable = 0; //

  bool testing = true;
  while (testing) {
    contextp->timeInc(1);
    top->clk = !top->clk; // clock toggle
    top->eval();
#if VM_TRACE
    tfp->dump(contextp->time());
#endif
    if (contextp->time() < 8)
      continue;     // Resetting
    top->reset = 0; // Deassert reset

    if (top->Istop_active) {
      top->reset = 1; // Deassert reset
    }
    testing = !top->done;
  }
#if VM_TRACE
  tfp->close();
#endif
  delete top;
  delete contextp;
  return 0;
}
