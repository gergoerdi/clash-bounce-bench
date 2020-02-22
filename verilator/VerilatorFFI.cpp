#include "VBounce.h"
#include "verilated.h"
#include "VerilatorFFI.h"

#include <iostream>

void hello()
{
    std::cout << "Hello from the other side" << std::endl;
}

vluint64_t main_time = 0;

double sc_time_stamp ()
{
    return main_time;
}

void step(VBounce& top)
{
    if (main_time > 0) {
        top.RESET = 0;
    }

    top.CLK_25MHZ = true;
    top.eval();
    main_time++;
    top.CLK_25MHZ = false;
    top.eval();
    main_time++;
}
