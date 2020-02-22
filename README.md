1. Run the hand-translated C benchmark:

```
$ cd csim
$ make
$ ./Bounce
Hand-translated C, from C: 4192001 cycles, 61 ms
```

2. Run the Verilator C++ benchmark:

```
$ cd verilator
$ ./mk.sh
$ ./SimMain
Verilator, from C: 4192001 cycles, 125 ms
```

3. Run the Haskell benchmarks

```
$ stack build
$ stack run sim-hs
Clash: 4192001 cycles, 12799 ms

$ stack run sim-ffi
Hand-translated C, from Haskell: 4192001 cycles, 241 ms

$ stack run sim-verilator
Verilator, from Haskell: 4192000 cycles, 365 ms
```

4. Marvel at the Haskell SDL frontend with VGA signal interpreter

```
$ stack run sim-ffi-sdl
60 frames in 1820 ms, 33.0 fps
60 frames in 1745 ms, 34.4 fps
60 frames in 1885 ms, 31.8 fps
60 frames in 1961 ms, 30.6 fps
60 frames in 1779 ms, 33.7 fps

$ stack run sim-verilator-sdl
60 frames in 2948 ms, 20.4 fps
60 frames in 2609 ms, 23.0 fps
60 frames in 2524 ms, 23.8 fps
60 frames in 2593 ms, 23.1 fps
```
