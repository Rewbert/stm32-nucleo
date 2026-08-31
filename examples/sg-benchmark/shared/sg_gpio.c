/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#include "drivers/gpio.h"
#include "drivers/exti.h"
#include "drivers/nvic.h"
#include "drivers/rcc.h"

#include "firmware/boards/board.h"

#include "sg_gpio.h"

sg_gpio_pin_t sg_gpio_pins[SG_GPIO_NUM_PINS] = {
    [SG_PIN_0] = {
        .port = BOARD_GPIO_PORT_A,
        .pin  = 2,

        .gpio_cfg = { .mode      = GPIO_MODE_INPUT,
                      .pull      = GPIO_PULLDOWN,
                      .alternate = GPIO_AF0,
                    },
        .exti_cfg = { .port = EXTI_PORT_A,
                      .pin  = 2,
                      .edge = EXTI_EDGE_RISING,
                    },
    },
    [SG_PIN_1] = {
        .port = BOARD_GPIO_PORT_A,
        .pin  = 3,

        .gpio_cfg = { .mode      = GPIO_MODE_INPUT,
                      .pull      = GPIO_PULLDOWN,
                      .alternate = GPIO_AF0,
                    },
        .exti_cfg = { .port = EXTI_PORT_A,
                      .pin  = 3,
                      .edge = EXTI_EDGE_RISING,
                    },
    },
};

static void sg_gpio_create(void) {
    for (int i = 0; i < SG_GPIO_NUM_PINS; i++) {
        sg_gpio_pin_t *p = &sg_gpio_pins[i];
        board_gpio_create(&p->gpio, p->port, p->pin, &p->gpio_backend);
        board_exti_create(&p->exti, &p->exti_backend, p->exti_cfg.pin);
    }
}

void sg_gpio_init(void) {
    sg_gpio_create();

    /* Only the secure world should actually configure the peripherals. */
#if HAL_SECURE
    rcc_enable(board_rcc(), RCC_GPIOA);

    for (int i = 0; i < SG_GPIO_NUM_PINS; i++) {
        sg_gpio_pin_t *p = &sg_gpio_pins[i];
        gpio_init(&p->gpio, &p->gpio_cfg);
        gpio_set_security(&p->gpio, GPIO_NONSECURE);
        exti_init(&p->exti, &p->exti_cfg);
        exti_set_security(&p->exti, EXTI_NONSECURE);

        int irqn = exti_irqn(&p->exti);
        nvic_set_priority(irqn, 0);
        nvic_set_target_nonsecure(irqn);
        nvic_enable_irq(irqn);
    }
#endif
}
