#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/gpio.h"
#include "drivers/exti.h"
#include "drivers/nvic.h"
#include "drivers/rcc.h"

#include "firmware/boards/board.h"

#include "breadboard.h"

#include <stdint.h>

breadboard_button_t buttons[BREADBOARD_NUM_BUTTONS] = {
    { .port = BOARD_GPIO_PORT_A,
      .pin  = 2,
        
      .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                    .pull            = GPIO_PULLUP,
                    .alternate       = GPIO_AF0,
                  },
      .exti_cfg = { .port             = EXTI_PORT_A,
                    .pin              = 2,
                    .edge             = EXTI_EDGE_FALLING,
                  },
    },
{ .port = BOARD_GPIO_PORT_A,
    .pin  = 3,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 3,
                  .edge             = EXTI_EDGE_FALLING,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 5,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 5,
                  .edge             = EXTI_EDGE_FALLING,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 6,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 6,
                  .edge             = EXTI_EDGE_FALLING,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 7,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 7,
                  .edge             = EXTI_EDGE_FALLING,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 8,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 8,
                  .edge             = EXTI_EDGE_FALLING,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 10,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 10,
                  .edge             = EXTI_EDGE_FALLING,
                },
    },
};

void breadboard_create() {
    for(int i = 0; i < BREADBOARD_NUM_BUTTONS; i++) {
        breadboard_button_t *bb = &buttons[i];
        board_gpio_create(&bb->gpio, bb->port, bb->pin, &bb->gpio_backend);
        board_exti_create(&bb->exti, &bb->exti_backend, bb->exti_cfg.pin);
    }
}

void breadboard_init() {
    breadboard_create();

    // Only the secure world should actually configure the peripherals
#if HAL_SECURE
    /* Enable GPIOA before we configure the GPIOA buttons */
    rcc_enable(board_rcc(), RCC_GPIOA);

    for(int i = 0; i < BREADBOARD_NUM_BUTTONS; i++) {
        breadboard_button_t *bb = &buttons[i];
        gpio_init(&bb->gpio, &bb->gpio_cfg);
        gpio_set_security(&bb->gpio, GPIO_NONSECURE);
        exti_init(&bb->exti, &bb->exti_cfg);
        exti_set_security(&bb->exti, EXTI_NONSECURE);

        int irqn = exti_irqn(&bb->exti);
        nvic_set_priority(irqn, 0);
        nvic_set_target_nonsecure(irqn);
        nvic_enable_irq(irqn);
    }
#endif
}
