CPPFLAGS	= $(shell pkg-config --cflags verilator) -I_verilator
CXXFLAGS	= -fPIC -O3
VERILATOR_OBJS	= _verilator/verilated.o
VERILATOR_LIBS	= _verilator/VSim__ALL.a

all: _build/libVerilatorFFI.a _build/SimMain

clean:
	rm -f _build/libVerilatorFFI.a _build/VerilatorFFI.o _build/SimMain

.PHONY: all clean

_build/VerilatorFFI.o: VerilatorFFI.cpp VerilatorFFI.h VerilatorAPI.h

_build/libVerilatorFFI.a: _build/VerilatorFFI.o $(VERILATOR_OBJS) $(VERILATOR_LIBS)
	rm -f $@
	ar rcsT $@ $^

_build/SimMain: _build/SimMain.o _build/libVerilatorFFI.a
	$(CXX) -o $@ $^

_build/%.o : %.cpp
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

