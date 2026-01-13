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
    GPIO_PORT_H,
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

typedef enum {
    GPIO_MODE_INPUT = 0,
    GPIO_MODE_OUTPUT,
    GPIO_MODE_AF,
    GPIO_MODE_ANALOG,
} gpio_mode_t;

typedef enum {
    GPIO_AF0  = 0,
    GPIO_AF1  = 1,
    GPIO_AF2  = 2,
    GPIO_AF3  = 3,
    GPIO_AF4  = 4,
    GPIO_AF5  = 5,
    GPIO_AF6  = 6,
    GPIO_AF7  = 7,
    GPIO_AF8  = 8,
    GPIO_AF9  = 9,
    GPIO_AF10 = 10,
    GPIO_AF11 = 11,
    GPIO_AF12 = 12,
    GPIO_AF13 = 13,
    GPIO_AF14 = 14,
    GPIO_AF15 = 15,
} gpio_af_t;


#endif // CORE_GPIO_H