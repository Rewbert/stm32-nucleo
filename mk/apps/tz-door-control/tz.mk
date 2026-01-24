.PHONY: all

all: $(TDZA_SECURE_ELF) $(TDZA_NS_NONSECURE_ELF)

include mk/apps/tz-door-control/nonsecure.mk