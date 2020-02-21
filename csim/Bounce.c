#include <stdint.h>
#include <stdbool.h>

#include <stdio.h>

typedef uint64_t Bit;
typedef uint64_t Reset;
typedef uint64_t U8;
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
    Bit VGA_DE;
    U8 VGA_RED;
    U8 VGA_GREEN;
    U8 VGA_BLUE;
} OUTPUT;

#define MASK(n)            ((1 << (n)) - 1)
#define SLICE(x, hi, lo)   (((x) >> (lo)) & MASK(((hi) - (lo) + 1)))
#define SEXT(x, n)         ((x) | (((x) & 1 << (n)) ? (-1 << n) : 0))
#define SLICE_S(x, hi, lo) SEXT(SLICE(x, hi, lo), (hi) - (lo))

#define MK_S11_S11(x, y) ((uint32_t)(SLICE((x), 10, 0)) << 11 | (uint32_t)(SLICE((y), 10, 0)))
#define MK_S10_S10(x, y) ((uint32_t)(SLICE((x), 9, 0)) << 10 | (uint32_t)(SLICE((y), 9, 0)))

void Bounce(INPUT* input, OUTPUT* output)
{
    // vgaDriver
    static VGAState1 stateH = (0 << 10) | 0;
    static VGAState2 stateV = (0 << 9) | 0;

    bool endLine = SLICE(stateH, 11, 10) == 3 && SLICE(stateH, 9, 0) == 47;

    VGAState1 stateH_ =
        SLICE(stateH, 11, 10) == 0 ? (SLICE(stateH, 9, 0) == 639 ? ((1 << 10) | 0) : ((0 << 10) | SLICE(stateH, 9, 0) + 1)) :
        SLICE(stateH, 11, 10) == 1 ? (SLICE(stateH, 9, 0) == 15 ? ((2 << 10) | 0) : ((1 << 10) | SLICE(stateH, 9, 0) + 1)) :
        SLICE(stateH, 11, 10) == 2 ? (SLICE(stateH, 9, 0) == 95 ? ((3 << 10) | 0) : ((2 << 10) | SLICE(stateH, 9, 0) + 1)) :
        SLICE(stateH, 11, 10) == 3 ? (SLICE(stateH, 9, 0) == 47 ? ((0 << 10) | 0) : ((3 << 10) | SLICE(stateH, 9, 0) + 1)) :
        0;

    VGAState2 stateV_ =
        (! endLine) ? stateV :
        SLICE(stateV, 10, 9) == 0 ? (SLICE(stateV, 8, 0) == 479 ? ((1 << 9) | 0) : ((0 << 9) | SLICE(stateV, 8, 0) + 1)) :
        SLICE(stateV, 10, 9) == 1 ? (SLICE(stateV, 8, 0) == 10 ? ((2 << 9) | 0) : ((1 << 9) | SLICE(stateV, 8, 0) + 1)) :
        SLICE(stateV, 10, 9) == 2 ? (SLICE(stateV, 8, 0) == 1 ? ((3 << 9) | 0) : ((2 << 9) | SLICE(stateV, 8, 0) + 1)) :
        SLICE(stateV, 10, 9) == 3 ? (SLICE(stateV, 8, 0) == 30 ? ((0 << 9) | 0) : ((3 << 9) | SLICE(stateV, 8, 0) + 1)) :
        0;

    M_I640 vgaX = SLICE(stateH, 11, 10) == 0 ?
        ((1 << 10) | SLICE(stateH, 9, 0)) : 0;
    Bit vgaSync_vgaHSync = SLICE(stateH, 11, 10) == 2 ?
        false : true;

    M_I480 vgaY = SLICE(stateV, 10, 9) == 0 ?
        ((1 << 9) | SLICE(stateV, 8, 0)) : 0;
    Bit vgaSync_vgaVSync = SLICE(stateV, 10, 9) == 2 ?
        false : true;

    Bit vgaSync_vgaDE = SLICE(vgaX, 10, 10) && SLICE(vgaY, 9, 9);

    static bool frameEnd_buf = false;
    bool frameEnd_buf_ = SLICE(vgaY, 9, 9);
    bool frameEnd = frameEnd_buf && !frameEnd_buf_;

    static S11_S11 ballX_speedX = (0 << 11) | (3 << 0);

    // bounceBetween1
    S11 leftWall = 639 - 15;
    S11 ballX = SLICE_S(ballX_speedX, 21, 11);
    S11 speedX = SLICE_S(ballX_speedX, 10, 0);
    S11_S11 move1 = MK_S11_S11(ballX + speedX, speedX);

    S11_S11 reflect11;
    {
        S11 reflect_p = leftWall;
        S11 reflect_n = -1;
        S11 reflect_x = SLICE_S(move1, 21, 11);
        S11 reflect_dx = SLICE_S(move1, 10, 0);
        S11 reflect_diff = reflect_p - reflect_x;
        bool sameDir =
            ((reflect_n < 0 ? -1 : reflect_n == 0 ? 0 : 1) ==
             (reflect_diff < 0 ? -1 : reflect_diff == 0 ? 0 : 1));

        reflect11 = sameDir ?
            MK_S11_S11(reflect_p + reflect_diff, -1 * reflect_dx) :
            MK_S11_S11(reflect_x, reflect_dx);
    }

    S11_S11 reflect12;
    {
        S11 reflect_p = 0;
        S11 reflect_n = 1;
        S11 reflect_x = SLICE_S(reflect11, 21, 11);
        S11 reflect_dx = SLICE_S(reflect11, 10, 0);
        S11 reflect_diff = reflect_p - reflect_x;
        bool sameDir =
            ((reflect_n < 0 ? -1 : reflect_n == 0 ? 0 : 1) ==
             (reflect_diff < 0 ? -1 : reflect_diff == 0 ? 0 : 1));

        reflect12 = sameDir ?
            MK_S11_S11(reflect_p + reflect_diff, -1 * reflect_dx) :
            MK_S11_S11(reflect_x, reflect_dx);
    }

    S11_S11 bounceBetween1 = reflect12;

    S11_S11 ballX_speedX_ =
        (!frameEnd) ? ballX_speedX :
        bounceBetween1;

    static S10_S10 ballY_speedY = (0 << 11) | (2 << 0);

    // bounceBetween2
    S10 bottomWall = 479 - 15;
    S11 ballY = SLICE_S(ballY_speedY, 19, 10);
    S11 speedY = SLICE_S(ballY_speedY, 9, 0);
    S10_S10 move2 = MK_S10_S10(ballY + speedY, speedY);

    S10_S10 reflect21;
    {
        S10 reflect_p = bottomWall;
        S10 reflect_n = -1;
        S10 reflect_x = SLICE_S(move2, 19, 10);
        S10 reflect_dx = SLICE_S(move2, 9, 0);
        S10 reflect_diff = reflect_p - reflect_x;
        bool sameDir =
            ((reflect_n < 0 ? -1 : reflect_n == 0 ? 0 : 1) ==
             (reflect_diff < 0 ? -1 : reflect_diff == 0 ? 0 : 1));

        reflect21 = sameDir ?
            MK_S10_S10(reflect_p + reflect_diff, -1 * reflect_dx) :
            MK_S10_S10(reflect_x, reflect_dx);
    }


    S10_S10 reflect22;
    {
        S10 reflect_p = 0;
        S10 reflect_n = 1;
        S10 reflect_x = SLICE_S(reflect21, 19, 10);
        S10 reflect_dx = SLICE_S(reflect21, 9, 0);
        S10 reflect_diff = reflect_p - reflect_x;
        bool sameDir =
            ((reflect_n < 0 ? -1 : reflect_n == 0 ? 0 : 1) ==
             (reflect_diff < 0 ? -1 : reflect_diff == 0 ? 0 : 1));

        reflect22 = sameDir ?
            MK_S10_S10(reflect_p + reflect_diff, -1 * reflect_dx) :
            MK_S10_S10(reflect_x, reflect_dx);
    }

    S10_S10 bounceBetween2 = reflect22;

    S10_S10 ballY_speedY_ =
        (!frameEnd) ? ballY_speedY :
        bounceBetween2;

    // isBall
    bool nearX =
        SLICE(vgaX, 10, 10) == 0 ? false :
        (ballX <= SLICE(vgaX, 9, 0) && SLICE(vgaX, 9, 0) < (ballX + 15));
    bool nearY =
        SLICE(vgaY, 9, 9) == 0 ? false :
        (ballY <= SLICE(vgaY, 8, 0) && SLICE(vgaY, 8, 0) < (ballY + 15));
    U24 bouncingBall = (nearX && nearY) ? 0xf0e040 : 0x303030;

    // vgaOut
    U24 vgaOut_mux = (! vgaSync_vgaDE) ? 0x000000 : bouncingBall;

    U8 vgaOut_vgaR = SLICE(vgaOut_mux, 23, 16);
    U8 vgaOut_vgaG = SLICE(vgaOut_mux, 15, 8);
    U8 vgaOut_vgaB = SLICE(vgaOut_mux, 7, 0);

    output->VGA_HSYNC = vgaSync_vgaHSync;
    output->VGA_VSYNC = vgaSync_vgaVSync;
    output->VGA_DE = vgaSync_vgaDE;
    output->VGA_RED = vgaOut_vgaR;
    output->VGA_GREEN = vgaOut_vgaG;
    output->VGA_BLUE = vgaOut_vgaB;

    stateH = input->RESET ? ((0 << 10) | 0) : stateH_;
    stateV = input->RESET ? ((0 << 9) | 0) : stateV_;
    frameEnd_buf = input->RESET ? false : frameEnd_buf_;
    ballX_speedX = input->RESET ? (0 << 11) | (3 << 0) : ballX_speedX_;
    ballY_speedY = input->RESET ? (0 << 10) | (2 << 0) : ballY_speedY_;

    /* if (frameEnd) */
    /*     printf("%3ld %3ld\n", ballX, ballY); */
}

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
