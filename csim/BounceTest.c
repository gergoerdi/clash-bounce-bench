#include "Bounce.h"

#include <stdio.h>

int main (int argc, char **argv)
{
    INPUT input;
    input.RESET = false;
    OUTPUT output;

    int i = 0;
    int cycles = 0;

    for (int j = 0; j < 60; ++j)
    {
        for (;; ++i)
        {
            Bounce(&input, &output);
            cycles++;
            if (output.VGA_HSYNC == 0 && output.VGA_VSYNC == 0) break;
        }

        for (;; ++i)
        {
            Bounce(&input, &output);
            cycles++;
            if (output.VGA_DE) break;
        }
    }

    printf("%d cycles\n", cycles);
}
