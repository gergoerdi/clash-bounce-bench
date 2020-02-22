#include <stdint.h>
#include <stdbool.h>

typedef int Bool;
typedef Bool Bit;
typedef Bit Reset;
typedef uint8_t U8;
typedef uint64_t M_I640;
typedef uint64_t M_I480;
typedef uint64_t U24;
typedef uint64_t VGAState1;
typedef uint64_t VGAState2;
typedef int64_t S11;
typedef uint64_t S11_S11;
typedef int64_t S10;
typedef uint64_t S10_S10;

typedef struct
{
    Reset RESET;
} INPUT;

typedef struct
{
    Bit VGA_HSYNC;
    Bit VGA_VSYNC;
    bool VGA_DE;
    U8 VGA_RED;
    U8 VGA_GREEN;
    U8 VGA_BLUE;
} OUTPUT;

void Bounce(const INPUT* input, OUTPUT* output);
