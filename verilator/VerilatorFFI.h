#include "VBounce.h"
#include "verilated.h"
#include "VerilatorAPI.h"

extern "C" {
    VBounce* vinit();
    void vstep(VBounce* top, const INPUT* input, OUTPUT* output);
    void vshutdown(VBounce* top);
}
