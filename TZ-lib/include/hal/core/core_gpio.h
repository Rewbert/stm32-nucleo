#ifndef CORE_GPIO_H
#define CORE_GPIO_H

#include <stdint.h>

typedef enum {
    LOW=0,
    HIGH,
} gpio_level_t;

typedef enum {
    GPIO_PORT_A=0,
    GPIO_PORT_B,
    GPIO_PORT_C,
    GPIO_PORT_D,
    GPIO_PORT_E,
    GPIO_PORT_F,
    GPIO_PORT_G,
} gpio_port_t;

typedef struct {
    gpio_port_t port;
    uint8_t pin;
} gpio_t;

#define GPIO(port, pin) ((gpio_t) { GPIO_PORT_##port, (pin) })

typedef enum {
    GPIO_NOPULL=0,
    GPIO_PULLUP,
    GPIO_PULLDOWN,
} gpio_pupdr_t;

#endif // CORE_GPIO_H