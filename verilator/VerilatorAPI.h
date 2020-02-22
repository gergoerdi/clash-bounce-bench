#include <stdint.h>

typedef struct
{
    uint8_t RESET;
} INPUT;

typedef struct
{
    uint8_t VGA_HSYNC;
    uint8_t VGA_VSYNC;
    uint8_t VGA_DE;
    uint8_t VGA_RED;
    uint8_t VGA_GREEN;
    uint8_t VGA_BLUE;
} OUTPUT;
