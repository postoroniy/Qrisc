export PATH:=/home/slava/projects/bluespec/bsc/inst/bin:$(PATH)

DISTRO ?= .
BSC=bsc
BSC_PATHS = $(DISTRO)::%/Libraries

BSC_COMPILATION_FLAGS ?= -keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps -check-assert \
-suppress-warnings G0020 +RTS -K128M -RTS  -show-range-conflict

## Default simulator is iverilog
VSIM = -vsim iverilog

compile:
# bsc -u $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -remove-dollar -g qrisc32 -verilog  qrisc32.bsv
	bsc -u $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -remove-dollar -g qrisc32_IF -verilog  qrisc32_IF.bsv
	bsc -u $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -remove-dollar -g qrisc32_ID -verilog  qrisc32_ID.bsv
# bsc -u $(BSC_COMPILATION_FLAGS) -p $(BSC_PATHS) -remove-dollar -g qrisc32_EX -verilog  qrisc32_EX.bsv

clean:
	rm -rf ./*.bo ./*.ba ./*.v
