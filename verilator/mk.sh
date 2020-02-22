set -xe

mkdir -p _build

(cd _build; stack exec -- clash -i../../src -outputdir clash --verilog ../../src/Bounce.hs)

verilator -cc -CFLAGS -O3 --clk CLK_25MHZ --exe SimMain.cpp -Mdir _build _build/clash/verilog/Bounce/Bounce/Bounce.v

(cd _build && make -f VBounce.mk)
