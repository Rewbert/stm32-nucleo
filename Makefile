.PHONY: all

all: secure.elf nonsecure.elf main.elf mhs.elf

include mk/TZ/tz.mk
include mk/app/main.mk
include mk/app/mhs.mk

clean:
	rm -r build
	rm *.elf