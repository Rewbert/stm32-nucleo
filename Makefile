.PHONY: all

all: secure.elf nonsecure.elf main.elf mhs.elf

include mk/TZ/tz.mk
include mk/app/main.mk
include mk/app/mhs.mk

clean:
	rm -r build
	rm *.elf

PROGRAMMER=openocd
PROGRAMMER_FLAGS=-f interface/stlink.cfg -f target/stm32l5x.cfg

flash: $(FILE)
	test -n "$(FILE)" || (echo "Usage: make flash FILE=main.elf" && exit 1)
	$(PROGRAMMER) $(PROGRAMMER_FLAGS) -c "program $(FILE) verify reset exit"