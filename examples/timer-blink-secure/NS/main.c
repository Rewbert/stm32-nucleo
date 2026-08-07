#include "firmware/boards/board.h"

/*
 * The secure world owns the timer and the LED, and never returns from its
 * main() (see S/main.c) — so this side never actually runs. It only exists
 * because the build produces a secure/non-secure pair.
 */
void main(void) {
    board_init();
    while (1) {
    }
}
