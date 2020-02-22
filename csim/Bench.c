#include "Bounce.h"

#include <stdio.h>
#include <time.h>

int main (int argc, char **argv)
{
    INPUT input;
    input.RESET = false;
    OUTPUT output;

    int cycles = 0;

    clock_t t0 = clock();
    for (int i = 0; i < 10; ++i)
    {
        for (;;)
        {
            Bounce(&input, &output);
            ++cycles;
            if (output.VGA_HSYNC == 0 && output.VGA_VSYNC == 0) break;
        }

        for (;;)
        {
            Bounce(&input, &output);
            ++cycles;
            if (output.VGA_DE == 1) break;
        }
    }
    clock_t t = clock();
    int ms = (((double)(t - t0))/CLOCKS_PER_SEC) * 1000;

    printf("Hand-translated C, from C: %d cycles, %d ms\n", cycles, ms);
}
