#include "Bounce.h"

#include <stdio.h>

int main (int argc, char **argv)
{
    INPUT input;
    input.RESET = false;
    OUTPUT output;

    int i = 0;

    for (int j = 0; j < 60; ++j)
    {
        for (;; ++i)
        {
            Bounce(&input, &output);
            if (!output.VGA_VSYNC) break;
        }

        for (;; ++i)
        {
            Bounce(&input, &output);
            if (output.VGA_VSYNC) break;
        }
    }
}
