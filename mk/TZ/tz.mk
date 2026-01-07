.PHONY: all

all: secure.elf nonsecure.elf

include mk/TZ/S/tz-secure.mk
include mk/TZ/NS/tz-nonsecure.mk