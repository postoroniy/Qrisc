///////////////////////////////////////////////////////////////////////////////
//    Project Qrisc32 is risc cpu implementation, purpose is studying
//    Digital System Design course at Kyoung Hee University during my PhD earning
//    Copyright (C) 2010-2023  Viacheslav Vinogradov
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
///////////////////////////////////////////////////////////////////////////////

#include "Vqrisc32_tb.h"
#include <memory.h>
#include <verilated.h>
#if VM_TRACE
#include "verilated_fst_c.h"
#endif

int main(int argc, char **argv, char **env)
{
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
  while (testing)
  {
    contextp->timeInc(1);
    top->clk = !top->clk; // clock toggle
    top->eval();
#if VM_TRACE
    tfp->dump(contextp->time());
#endif
    if (contextp->time() < 8)
      continue;     // Resetting
    top->reset = 0; // Deassert reset

    if (top->Istop_active)
    {
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
