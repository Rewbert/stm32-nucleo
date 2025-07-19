/* These GPIO operations assume now that the GPIO is controlling a LED. Perhaps the setup should be
more general (not all GPIOs are LED's, of course). TODO */

#include "stm32l5xx.h"
#include "gpio.h"

MAKE_GPIO(red_led,   A, 9) // PA9
MAKE_GPIO(blue_led,  B, 7) // PB7
MAKE_GPIO(green_led, C, 7) // PC7

int is_port_enabled(struct GPIO *gpio) {
    uint32_t current_value = *((uint32_t *) gpio->enable_register);
    return (current_value >> gpio->enable_position) & 1;
}

// TODO add inverse, to turn it off to save power
void enable_port(struct GPIO *gpio) {
    volatile int dummy;

    *(uint32_t *) gpio->enable_register |= (1 << gpio->enable_position);
    dummy = gpio->enable_position;
    dummy = gpio->enable_position;
}

// there are 4 different modes, 00 input mode, 01 general purpose output mode, 10 alternate function mode, and 11 analog mode
void set_output_mode(struct GPIO *gpio) {
    uint32_t *reg = (uint32_t *) &(gpio->gpio->MODER);
    *reg &= ~(gpio->mode_msk);
    *reg |= (1 << gpio->mode_pos); // this 1, if it is replaced with something else we set a different mode
}

void initialise_led(struct GPIO *led) {
    if (! is_port_enabled(led) ) {
        enable_port(led);
    }

    set_output_mode(led);
}

int read_led(struct GPIO *led) {
    return (led->gpio->ODR >> led->pin) & 1;
}

void set_led(struct GPIO *led, int val) {
    led->gpio->ODR |= (val << led->pin);
}

void toggle_led(struct GPIO *led) {
    led->gpio->ODR ^= (1 << led->pin);
}