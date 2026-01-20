#ifndef CORE_EXTI_H
#define CORE_EXTI_H

#include "hal/core/gpio.h"

typedef enum {
    EXTI_EDGE_RISING,
    EXTI_EDGE_FALLING,
    EXTI_EDGE_BOTH,
} exti_edge_t;

typedef enum {
    EXTI_PORTCODE_A,
    EXTI_PORTCODE_B,
    EXTI_PORTCODE_C,
    EXTI_PORTCODE_D,
    EXTI_PORTCODE_E,
    EXTI_PORTCODE_F,
    EXTI_PORTCODE_G,
    EXTI_PORTCODE_H,
} exti_portcode_t;

typedef struct {
    gpio_port_t port;
    uint8_t     pin;   // 0–15
} exti_line_t;

typedef void (*exti_callback_t)(exti_edge_t edge);

#endif // CORE_EXTI_H