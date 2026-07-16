PWD := $(shell pwd)
SCALA_SRC := $(shell find src -name "*.scala")
SIMMODELS_DIR ?= $(abspath ../Zircon-SimModels)
all: verilog

USE_SBT := 1

verilog: $(SCALA_SRC)
ifeq ($(USE_SBT), 1)
	@BUILD_MODE=SYNC sbt 'runMain Main' --batch
else
	@BUILD_MODE=SYNC ./mill -s -j0 _.runMain Main
endif

sim-verilog: $(SCALA_SRC)
ifeq ($(USE_SBT), 1)
	@BUILD_MODE=sim sbt 'runMain Main' --batch
else
	@BUILD_MODE=sim ./mill -s -j0 _.runMain Main
endif


run: sim-verilog
	@$(MAKE) -C sim run IMG="$(IMG)" ARGS="$(ARGS)" SIMMODELS_DIR="$(SIMMODELS_DIR)"
# ifeq ($(USE_SBT), 1)
# 	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir BUILD_MODE=sim sbt 'testOnly EmuMain' --batch
# else
# 	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir BUILD_MODE=sim ./mill -s -j0 _.test.testOnly EmuMain
# endif
	
