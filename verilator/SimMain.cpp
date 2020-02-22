#include "VerilatorFFI.h"

#include <iostream>
#include <time.h>

int main(int argc, char** argv, char** env)
{
    VBounce* top = init();

    INPUT input;
    OUTPUT output;

    input.RESET = 1;
    step(top, &input, &output);
    input.RESET = 0;

    int cycles = 0;

    clock_t t0 = clock();
    for (int j = 0; j < 10; ++j)
    {
        for (;;)
        {
            step(top, &input, &output);
            cycles++;
            if (output.VGA_HSYNC == 0 && output.VGA_VSYNC == 0) break;
        }

        for (;;)
        {
            step(top, &input, &output);
            cycles++;
            if (output.VGA_DE) break;
        }
    }
    clock_t t = clock();
    int ms = (((double)(t - t0))/CLOCKS_PER_SEC) * 1000;

    printf("Verilator, from C: %d cycles, %d ms\n", cycles, ms);
    return 0;
}
