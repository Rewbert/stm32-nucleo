#ifndef FIRMWARE_PROFILE_H
#define FIRMWARE_PROFILE_H

#include <stdint.h>

#include "drivers/gpio.h"
#include "firmware/boards/board.h"

/**
 * @brief Reusable GPIO code emitter for profiling with a logic analyser.
 *
 * Drives 4 GPIOs as a single nibble in one register write, so dropping a
 * timing marker into code costs one function call and one BSRR write.
 *
 * Pins: Arduino D15/D14/D36/D35 = PB8/PB9/PB10/PB11, identical on the
 * STM32L5 and STM32U5 Nucleo-144 boards (same Zio connector pinout, per
 * UM2581/UM2861). This is a contiguous nibble on one port, and none of these
 * pins are used elsewhere in the project (console, LEDs, button, keypad,
 * solenoid).
 *
 *   bit 0 -> PB8  (Arduino D15)
 *   bit 1 -> PB9  (Arduino D14)
 *   bit 2 -> PB10 (Arduino D36)
 *   bit 3 -> PB11 (Arduino D35)
 */

#define PROFILE_NUM_PINS 4
#define PROFILE_MAX_CODE ((1u << PROFILE_NUM_PINS) - 1) /* 15 */

typedef struct {
    gpio_dev_t                pins[PROFILE_NUM_PINS];
    /* The batch-writer expects an array of pointers to gpio_dev_t
     * By creating this field to mirror the gpio_dev_t above,
     * we avoid having to reconstruct this ** at runtime.
     */
    gpio_dev_t                *pin_ptrs[PROFILE_NUM_PINS];
    board_gpio_backend_t       pin_backends[PROFILE_NUM_PINS];
    gpio_port_dev_t            port;
    board_gpio_port_backend_t  port_backend;
} profile_dev_t;

/**
 * @brief Enable the GPIOB clock, configure the 4 profiling pins as outputs,
 * all low, and release them to the non-secure world. Call once from secure
 * code, after board_init() -- this is what makes the pins usable from both
 * worlds.
 *
 * Call once from non-secure code too, also after board_init(): the
 * non-secure call sets up its own `gpio_dev_t`s for the same (by then
 * non-secure) pins so non-secure code can call profile_emit(). The release
 * step is a no-op when compiled into the non-secure image.
 */
void profile_init(profile_dev_t *prof);

/**
 * @brief Drive the profiling pins to the low nibble of `code` in a single
 * register write. Only the low PROFILE_NUM_PINS bits of `code` are used.
 */
void profile_emit(profile_dev_t *prof, uint8_t code);

#endif // FIRMWARE_PROFILE_H
