SRC_DIR = ./src
INC_DIR = ./include
BIN_DIR = ./bin
REL_DIR = ./release

SRC = $(wildcard $(SRC_DIR)/*.c) $(wildcard $(SRC_DIR)/*/*.c)
OUTPUT = $(BIN_DIR)/bin.exe

CFLAGS = -Wall -Wextra -O0 -g3 -std=c99 #-Os -s -static -fdata-sections -ffunction-sections -Wl,--gc-sections

CC = gcc

all:
	$(CC) $(SRC) -o $(OUTPUT) -I$(INC_DIR) $(CFLAGS) #-DSMOL_DEBUG_MODE
	
#-------------------------------------------------

VALGRIND_OUT = ./valgrind/analysis.out
CALLGRIND_FLAGS = --tool=callgrind --dump-line=yes --dump-instr=yes --collect-jumps=yes --collect-systime=yes --collect-bus=yes --cache-sim=yes --branch-sim=yes --simulate-wb=yes --simulate-hwpref=yes --cacheuse=yes --time-stamp=yes

mcall:
	make all
	rm -f $(VALGRIND_OUT)
	valgrind $(CALLGRIND_FLAGS) --callgrind-out-file=$(VALGRIND_OUT) $(OUTPUT)
	sudo kcachegrind $(VALGRIND_OUT)