set -xe

mkdir -p _build

(cd _build; stack exec -- clash -i../../src -outputdir clash --verilog ../../src/Bounce.hs)

sh verilate.sh
make
