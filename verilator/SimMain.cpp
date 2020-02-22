#include "VerilatorFFI.h"

#include <iostream>
#include <time.h>

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
