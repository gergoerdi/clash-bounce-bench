CLASH_SRC	= Bounce.hs
CLASH_DIR	= ../src
VERILOG_SRCS	= ../_build/clash-syn/Bounce.topEntity/topEntity.v

OUT_ROOT	= ../_build
SYN_DIR		= $(OUT_ROOT)/clash-syn
SIM_DIR		= $(OUT_ROOT)/verilator
VERILATOR_HDRS	= $(SIM_DIR)/VSim.h
VERILATOR_LIB	= $(SIM_DIR)/VSim__ALL.a
VERILATOR_OBJS  = $(SIM_DIR)/verilated.o

CPPFLAGS	= $(shell pkg-config --cflags verilator) -I$(SIM_DIR)
CXXFLAGS	= -fPIC -O3
VERILATOR_FLAGS	= -CFLAGS '-O3 -fPIC' -Wno-fatal --prefix VSim +1364-2001ext+v

# THREADS := 1

ifdef THREADS
VERILATOR_OBJS  += $(SIM_DIR)/verilated_threads.o
CPPFLAGS	+= -DVL_THREADED=1
VERILATOR_FLAGS	+= --threads $(THREADS)
endif

all: $(SIM_DIR)/libVerilatorFFI.a $(SIM_DIR)/SimMain

clean:
	rm -rf $(SYN_DIR)
	rm -rf $(SIM_DIR)

.PHONY: all clean

$(VERILOG_SRCS): $(CLASH_DIR)/$(CLASH_SRC)
	stack exec -- clash -i$(CLASH_DIR) -outputdir $(SYN_DIR) --verilog $(CLASH_DIR)/$(CLASH_SRC)

$(SIM_DIR)/VSim.mk: $(VERILOG_SRCS)
	verilator $(VERILATOR_FLAGS) --cc -Mdir $(SIM_DIR) $(VERILOG_SRCS)

$(SIM_DIR)/verilated.o: $(shell pkg-config --variable=includedir verilator)/verilated.cpp
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

$(SIM_DIR)/verilated_threads.o: $(shell pkg-config --variable=includedir verilator)/verilated_threads.cpp
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

$(VERILATOR_LIB) $(VERILATOR_HDRS): $(SIM_DIR)/VSim.mk 
	cd $(SIM_DIR) && make -f VSim.mk

$(SIM_DIR)/VerilatorFFI.o: VerilatorFFI.cpp VerilatorFFI.h VerilatorAPI.h $(VERILATOR_HDRS)

$(SIM_DIR)/libVerilatorFFI.a: $(SIM_DIR)/VerilatorFFI.o $(VERILATOR_LIB) $(VERILATOR_OBJS)
	rm -f $@
	mkdir -p $(SIM_DIR)
	ar rcsT $@ $^

$(SIM_DIR)/SimMain: $(SIM_DIR)/SimMain.o $(SIM_DIR)/libVerilatorFFI.a
	$(CXX) -o $@ $^

$(SIM_DIR)/%.o : %.cpp
	mkdir -p $(SIM_DIR)
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

