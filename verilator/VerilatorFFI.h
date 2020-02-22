#include "VBounce.h"
#include "verilated.h"
#include "VerilatorAPI.h"

extern "C" {
    void hello();

    VBounce* init();
    void step(VBounce* top, const INPUT* input, OUTPUT* output);
    void shutdown(VBounce* top);
}
