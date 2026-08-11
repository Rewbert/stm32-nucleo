/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#include "domain/domain.h"

#include "drivers/gpio.h"
#include "drivers/exti.h"
#include "drivers/nvic.h"
#include "drivers/rcc.h"

#include "firmware/boards/board.h"

#include "keypad.h"

keypad_key_t keypad_keys[KEYPAD_NUM_KEYS] = {
    { .pin = 0, .digit = 1 },
    { .pin = 1, .digit = 2 },
    { .pin = 2, .digit = 3 },
    { .pin = 3, .digit = 4 },
};

void keypad_init(void) {
    for (int i = 0; i < KEYPAD_NUM_KEYS; i++) {
        keypad_key_t *k = &keypad_keys[i];
        board_gpio_create(&k->gpio, BOARD_GPIO_PORT_D, k->pin, &k->gpio_backend);
        board_exti_create(&k->exti, &k->exti_backend, k->pin);
    }

#if HAL_SECURE
    rcc_enable(board_rcc(), RCC_GPIOD);

    gpio_config_t gpio_cfg = {
        .mode      = GPIO_MODE_INPUT,
        .pull      = GPIO_PULLDOWN,
        .alternate = GPIO_AF0,
    };

    for (int i = 0; i < KEYPAD_NUM_KEYS; i++) {
        keypad_key_t *k = &keypad_keys[i];

        gpio_init(&k->gpio, &gpio_cfg);
        gpio_set_security(&k->gpio, GPIO_NONSECURE);

        exti_config_t exti_cfg = {
            .port = EXTI_PORT_D,
            .pin  = k->pin,
            .edge = EXTI_EDGE_RISING,
        };
        exti_init(&k->exti, &exti_cfg);
        exti_set_security(&k->exti, EXTI_NONSECURE);

        int irqn = exti_irqn(&k->exti);
        nvic_set_priority(irqn, 0);
        nvic_set_target_nonsecure(irqn);
        nvic_enable_irq(irqn);
    }
#endif
}
