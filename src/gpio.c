
#include "stm32l5xx.h"
#include "gpio.h"

MAKE_GPIO(red_led,   A, 9)
MAKE_GPIO(blue_led,  B, 7)
MAKE_GPIO(green_led, C, 7)

int is_port_enabled(struct GPIO *gpio) {
    uint32_t current_value = *((uint32_t *) gpio->enable_register);
    return (current_value >> gpio->enable_position) & 1;
}

void enable_port(struct GPIO *gpio) {
    volatile int dummy;

    *(uint32_t *) gpio->enable_register |= (1 << gpio->enable_position);
    dummy = gpio->enable_position;
    dummy = gpio->enable_position;
}

void set_output_mode(struct GPIO *gpio) {
    uint32_t *reg = (uint32_t *) &(gpio->gpio->MODER);
    *reg &= ~(gpio->mode_msk);
    *reg |= (1 << gpio->mode_pos);
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