///////////////////////////////////////////////////////////////////////////////
// AXI protocol smoke test wrapper.
///////////////////////////////////////////////////////////////////////////////

#include "Vqrisc32_axi_tb.h"
#include <verilated.h>
#if VM_TRACE
#include "verilated_vcd_c.h"
#endif

int main(int argc, char **argv, char **env)
{
  VerilatedContext *contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);
  Vqrisc32_axi_tb *top = new Vqrisc32_axi_tb{contextp};

#if VM_TRACE
  Verilated::traceEverOn(true);
  VerilatedVcdC *tfp = new VerilatedVcdC;
  top->trace(tfp, 99);
  tfp->open("build/vcd/qrisc32_axi_tb.vcd");
#endif

  top->areset = 1;
  top->verbose = 0;
  top->Istop_enable = 0;
  top->Dstop_enable = 0;

  bool testing = true;
  while (testing)
  {
    contextp->timeInc(1);
    top->clk = !top->clk;
    top->eval();
#if VM_TRACE
    tfp->dump(contextp->time());
#endif
    if (contextp->time() < 8)
      continue;
    top->areset = 0;

    testing = !top->done;
  }

#if VM_TRACE
  tfp->close();
#endif

  delete top;
  delete contextp;
  return 0;
}
