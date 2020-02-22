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

VBounce* init()
{
    // Verilated::commandArgs(0, 0);
    return new VBounce();
}

void shutdown(VBounce *top)
{
    delete top;
}

void step(VBounce* top, const INPUT* input, OUTPUT* output)
{
    top->RESET = input->RESET;

    top->CLK_25MHZ = true;
    top->eval();
    main_time++;
    top->CLK_25MHZ = false;
    top->eval();
    main_time++;

    output->VGA_HSYNC = top->VGA_HSYNC;
    output->VGA_VSYNC = top->VGA_VSYNC;
    output->VGA_DE = top->VGA_DE;
    output->VGA_RED = top->VGA_RED;
    output->VGA_GREEN = top->VGA_GREEN;
    output->VGA_BLUE = top->VGA_BLUE;
}
