set -xe

verilator -cc -CFLAGS "-O3 -fPIC" --clk CLK_25MHZ -Mdir _verilator-exe _build/clash/verilog/Bounce/Bounce/Bounce.v --exe DummyMain.cpp
(cd _verilator-exe && make -f VBounce.mk)

verilator -cc -CFLAGS "-O3 -fPIC" --clk CLK_25MHZ -Mdir _verilator-lib _build/clash/verilog/Bounce/Bounce/Bounce.v SimStub.cpp
(cd _verilator-lib && make -f VBounce.mk)