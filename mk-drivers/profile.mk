ifndef MK_DRIVERS_PROFILE
MK_DRIVERS_PROFILE := 1

include mk-drivers/board.mk

###
# Builds firmware/profile/profile.c, a reusable GPIO code-emitting helper for
# driving timing markers on a logic analyser (Arduino D15/D14/D36/D35 =
# PB8-PB11). Shared by examples/sg-benchmark and examples/microhastee -- an
# example opts in with `include mk-drivers/profile.mk` and links PROFILE_O /
# PROFILE_NS_O into its secure / non-secure image.
###

PROFILE_SRC := firmware/profile/profile.c

PROFILE_O    := build/s/firmware/profile/profile.o
PROFILE_NS_O := build/ns/firmware/profile/profile.o

build/s/firmware/profile/%.o: firmware/profile/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

build/ns/firmware/profile/%.o: firmware/profile/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

endif
