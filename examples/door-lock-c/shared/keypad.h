/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#ifndef KEYPAD_H
#define KEYPAD_H

#include <stdint.h>

#include "drivers/gpio.h"
#include "drivers/exti.h"

#include "firmware/boards/board.h"

/*
 * 4 free GPIOs (PD0..PD3), each with its own EXTI line -- not wired to a
 * physical keypad yet, to be hard-wired to a breadboard later (see the
 * example README). Each button is one digit (1-4) of the PIN, same board
 * wiring as examples/microhastee's Haskell DoorLock.hs, so the two examples
 * are a fair side-by-side comparison. Pull-down + rising edge matches this
 * board's own user-button convention (firmware/boards/stm32u5/board.c):
 * press drives the pin high.
 */

#define KEYPAD_NUM_KEYS 4

typedef struct {
    uint8_t pin;    /* PD0..PD3 */
    uint8_t digit;  /* 1..4 */

    board_gpio_backend_t gpio_backend;
    gpio_dev_t            gpio;

    board_exti_backend_t exti_backend;
    exti_dev_t            exti;
} keypad_key_t;

extern keypad_key_t keypad_keys[KEYPAD_NUM_KEYS];

/*
 * Both worlds call this: it always creates the GPIO/EXTI device descriptors.
 * Only the secure build (HAL_SECURE) actually configures the peripherals and
 * releases them to the non-secure world -- same split as
 * examples/breadboard-buttons/shared/breadboard.c.
 */
void keypad_init(void);

static inline gpio_dev_t *keypad_gpio(int idx) {
    return &keypad_keys[idx].gpio;
}

static inline exti_dev_t *keypad_exti(int idx) {
    return &keypad_keys[idx].exti;
}

#endif /* KEYPAD_H */
