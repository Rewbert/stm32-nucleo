.PHONY: all

all: secure.elf nonsecure.elf main.elf mhs.elf newmhs.elf nonsecure-dc.elf secure-main.elf

include mk/app/main.mk
include mk/app/mhs.mk
include mk/app/new-mhs.mk
include mk/apps/tz-door-control/tz.mk
include mk/apps/tz-door-control-macro/tz.mk
include mk/apps/main.mk

clean:
	rm -r build
	rm *.elf

#### flashing #####

BOARD ?= stm32l5

OPENOCD         = $(HOME)/OpenOCD/src/openocd
OPENOCD_SCRIPTS = $(HOME)/OpenOCD/tcl

OPENOCD_FLAGS_stm32l5 = -s $(OPENOCD_SCRIPTS) -f interface/stlink.cfg -f target/stm32l5x.cfg
# stlink-dap + dapdirect_swd needed for stm32u5; srst_nogate avoids TZ flash-probe issues
OPENOCD_FLAGS_stm32u5 = -s $(OPENOCD_SCRIPTS) \
    -f interface/stlink-dap.cfg \
    -c "transport select dapdirect_swd" \
    -f target/stm32u5x.cfg \
    -c "reset_config srst_nogate connect_assert_srst"

OPENOCD_FLAGS = $(OPENOCD_FLAGS_$(BOARD))

flash:
	test -n "$(FILE)" || (echo "Usage: make flash FILE=<elf> [BOARD=stm32l5|stm32u5]" && exit 1)
	$(OPENOCD) $(OPENOCD_FLAGS) -c "program $(FILE) verify reset exit"

flash_tz:
	test -n "$(SECURE)" || (echo "Usage: make flash_tz SECURE=secure.elf NONSECURE=nonsecure.elf [BOARD=stm32l5|stm32u5]" && exit 1)
	$(OPENOCD) $(OPENOCD_FLAGS) -c "init" -c "reset halt" -c "program $(SECURE) verify" -c "program $(NONSECURE) verify" -c "reset" -c "exit"

##### debugging #####
# run `make openocd` in one terminal, and in another `make debug FILE=<the elf>`, or `make debug_tz`

DEBUGGER     = arm-none-eabi-gdb
DEBUG_HOST   = localhost
DEBUG_PORT   = 3333
DEBUGGERFLAGS = -ex "target extended-remote $(DEBUG_HOST):$(DEBUG_PORT)"

openocd:
	$(OPENOCD) $(OPENOCD_FLAGS) -c "gdb_port $(DEBUG_PORT)" -c "init" -c "reset halt"

debug:
	test -n "$(FILE)" || (echo "Usage: make debug FILE=main.elf" && exit 1)
	$(DEBUGGER) $(DEBUGGERFLAGS) $(FILE)

debug_tz:
	test -n "$(SECURE)" || (echo "Usage: make debug_tz SECURE=secure.elf NONSECURE=nonsecure.elf" && exit 1)
	$(DEBUGGER) $(DEBUGGERFLAGS) $(SECURE) -ex "add-symbol-file $(NONSECURE)"

##### Option bytes management #####

read_option_bytes:
	STM32_Programmer_CLI -c port=SWD mode=UR -ob displ


fix:
	$(OPENOCD) -s $(OPENOCD_SCRIPTS) -f interface/stlink-dap.cfg -c "transport select dapdirect_swd" -f target/stm32u5x.cfg -c "reset_config srst_nogate connect_assert_srst" -c "init; reset run; exit"

# NOTE
# sometimes flashing breaks, and the board is bricked. Claude could figure
# out what the problem was, and claims to run this command to fix it

#   src/openocd -s tcl \                                                                                                                                                                        
#     -f interface/stlink-dap.cfg \                                                                                                                                                             
#     -c "transport select dapdirect_swd" \                                                                                                                                                     
#     -f target/stm32u5x.cfg \                                                                                                                                                                  
#     -c "reset_config srst_nogate connect_assert_srst" \                                                                                                                                       
#     -c "init; reset run; exit"