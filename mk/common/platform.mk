include mk/common/librobos.mk

PLATFORM_C = $(ROBOS)/src/syscalls.c $(ROBOS)/startup.c
PLATFORM_O = $(addprefix build/platform/, $(addsuffix .o,$(basename $(notdir $(PLATFORM_C)))))

build/platform/%.o: $(ROBOS)/src/%.c
	mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

build/platform/%.o: $(ROBOS)/%.c
	mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@