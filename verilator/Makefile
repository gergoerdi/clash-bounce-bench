CPPFLAGS	= $(shell pkg-config --cflags verilator) -I_verilator-lib
CXXFLAGS	= -fPIC
VERILATOR_OBJS	= _verilator-exe/verilated.o
VERILATOR_LIBS	= _verilator-lib/VBounce__ALL.a

all: libVerilatorFFI.a SimMain

clean:
	rm -f libVerilatorFFI.a VerilatorFFI.o

.PHONY: all clean

VerilatorFFI.o: VerilatorFFI.cpp VerilatorFFI.h

libVerilatorFFI.a: VerilatorFFI.o $(VERILATOR_OBJS) $(VERILATOR_LIBS)
	rm -f $@
	ar rcsT $@ $^

SimMain: SimMain.o libVerilatorFFI.a
	$(CXX) -o $@ $^
