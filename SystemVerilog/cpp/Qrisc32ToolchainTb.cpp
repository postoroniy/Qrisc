#include "Vqrisc32_toolchain_tb.h"
#include <memory.h>
#include <string>
#include <verilated.h>
#if VM_TRACE
#include "verilated_vcd_c.h"
#endif

static std::string plusarg_value(int argc, char **argv, const char *prefix, const char *fallback)
{
  const std::string key(prefix);
  for (int i = 1; i < argc; ++i)
  {
    const std::string arg(argv[i]);
    if (arg.rfind(key, 0) == 0)
      return arg.substr(key.size());
  }
  return fallback;
}

int main(int argc, char **argv, char **env)
{
  VerilatedContext *contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);
  Vqrisc32_toolchain_tb *top = new Vqrisc32_toolchain_tb{contextp};

#if VM_TRACE
  Verilated::traceEverOn(true);
  VerilatedVcdC *tfp = new VerilatedVcdC;
  top->trace(tfp, 99);
  const std::string vcd_path = plusarg_value(argc, argv, "+VCD=", "build/vcd/qrisc32_toolchain_tb.vcd");
  tfp->open(vcd_path.c_str());
#endif
  top->areset = 1;
  top->verbose = plusarg_value(argc, argv, "+VERBOSE=", "0") != "0";
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

    if (top->Istop_active)
      top->areset = 1;
    testing = !top->done;
  }
#if VM_TRACE
  tfp->close();
#endif
  delete top;
  delete contextp;
  return 0;
}
