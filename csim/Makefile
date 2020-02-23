OUT_ROOT	= ../_build
OUT_DIR  	= $(OUT_ROOT)/csim
CFLAGS	 	= -std=c99 -O3

$(OUT_DIR)/Bounce: $(OUT_DIR)/Bounce.o $(OUT_DIR)/Bench.o

$(OUT_DIR)/Bounce.o: Bounce.c Bounce.h

$(OUT_DIR)/Bench.o: Bench.c Bounce.h

$(OUT_DIR)/%.o : %.c
	mkdir -p $(OUT_DIR)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

