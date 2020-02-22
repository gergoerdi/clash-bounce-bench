#include "VBounce.h"
#include "verilated.h"

#include <iostream>
#include <time.h>

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

int main(int argc, char** argv, char** env)
{
    Verilated::commandArgs(argc, argv);
    VBounce top;

    top.RESET = 1;

    int cycles = 0;

    clock_t t0 = clock();
    for (int j = 0; j < 10; ++j)
    {
        for (;;)
        {
            step(top);
            cycles++;
            if (top.VGA_HSYNC == 0 && top.VGA_VSYNC == 0) break;
        }

        for (;;)
        {
            step(top);
            cycles++;
            if (top.VGA_DE) break;
        }
    }
    clock_t t = clock();
    int ms = (((double)(t - t0))/CLOCKS_PER_SEC) * 1000;

    printf("Verilator, from C: %d cycles, %d ms\n", cycles, ms);
    return 0;
}
