WORK_DIR := $(abspath .)
PROCESSOR_DIR := $(abspath ..)
VERILOG_DIR := $(PROCESSOR_DIR)/verilog
SIMMODELS_DIR ?= $(abspath $(PROCESSOR_DIR)/../../Zircon-SimModels)
BUILD_DIR := $(WORK_DIR)/build
BINARY := $(BUILD_DIR)/VCPU

export CCACHE_DISABLE := 1

VERILATOR_FLAGS := --cc --exe --build -j 4
VERILATOR_FLAGS += --top-module CPU --prefix VCPU --Mdir $(BUILD_DIR)
VERILATOR_FLAGS += -O3 -Wno-UNOPTFLAT -Wno-WIDTHEXPAND
VERILATOR_FLAGS += -I$(VERILOG_DIR)
VERILATOR_FLAGS += -CFLAGS "-std=c++17 -I$(SIMMODELS_DIR)/include"

CPP_SOURCES := $(WORK_DIR)/main.cpp
CPP_SOURCES += $(SIMMODELS_DIR)/src/DDR.cpp
CPP_SOURCES += $(SIMMODELS_DIR)/src/RVCPU.cpp

.PHONY: all clean run

all: $(BINARY)

$(BINARY): $(CPP_SOURCES) $(VERILOG_DIR)/CPU.sv
	verilator $(VERILATOR_FLAGS) \
		$(VERILOG_DIR)/CPU.sv \
		$(CPP_SOURCES)

run: $(BINARY)
	@test -n "$(IMG)" || (echo "IMG is required"; exit 2)
	$(BINARY) $(IMG) $(ARGS)

clean:
	rm -rf $(BUILD_DIR)
