#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/gpio.h"
#include "drivers/exti.h"

#include "board.h"

#include "breadboard.h"

#include <stdint.h>

breadboard_button_t buttons[BREADBOARD_NUM_BUTTONS] = {
    { .port = BOARD_GPIO_PORT_A,
      .pin  = 2,
        
      .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                    .pull            = GPIO_PULLUP,
                    .alternate       = GPIO_AF0,
                    .security_domain = GPIO_NONSECURE,
                  },
      .exti_cfg = { .port             = EXTI_PORT_A,
                    .pin              = 2,
                    .edge             = EXTI_EDGE_FALLING,
                    .priority         = 0,
                    .secure           = false,
                    .target_nonsecure = true,
                  },
    },
{ .port = BOARD_GPIO_PORT_A,
    .pin  = 3,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                  .security_domain = GPIO_NONSECURE,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 3,
                  .edge             = EXTI_EDGE_FALLING,
                  .priority         = 0,
                  .secure           = false,
                  .target_nonsecure = true,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 5,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                  .security_domain = GPIO_NONSECURE,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 5,
                  .edge             = EXTI_EDGE_FALLING,
                  .priority         = 0,
                  .secure           = false,
                  .target_nonsecure = true,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 6,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                  .security_domain = GPIO_NONSECURE,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 6,
                  .edge             = EXTI_EDGE_FALLING,
                  .priority         = 0,
                  .secure           = false,
                  .target_nonsecure = true,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 7,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                  .security_domain = GPIO_NONSECURE,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 7,
                  .edge             = EXTI_EDGE_FALLING,
                  .priority         = 0,
                  .secure           = false,
                  .target_nonsecure = true,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 8,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                  .security_domain = GPIO_NONSECURE,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 8,
                  .edge             = EXTI_EDGE_FALLING,
                  .priority         = 0,
                  .secure           = false,
                  .target_nonsecure = true,
                },
    },
    { .port = BOARD_GPIO_PORT_A,
    .pin  = 10,
        
    .gpio_cfg = { .mode            = GPIO_MODE_INPUT,
                  .pull            = GPIO_PULLUP,
                  .alternate       = GPIO_AF0,
                  .security_domain = GPIO_NONSECURE,
                },
    .exti_cfg = { .port             = EXTI_PORT_A,
                  .pin              = 10,
                  .edge             = EXTI_EDGE_FALLING,
                  .priority         = 0,
                  .secure           = false,
                  .target_nonsecure = true,
                },
    },
};

void breadboard_create() {
    for(int i = 0; i < BREADBOARD_NUM_BUTTONS; i++) {
        breadboard_button_t *bb = &buttons[i];
        board_gpio_create(&bb->gpio, bb->port, bb->pin, &bb->gpio_backend);
        board_exti_create(&bb->exti, &bb->exti_backend);
    }
}

void breadboard_init() {
    breadboard_create();

    // Only the secure world should actually configure the peripherals
#if HAL_SECURE
    for(int i = 0; i < BREADBOARD_NUM_BUTTONS; i++) {
        breadboard_button_t *bb = &buttons[i];
        gpio_init(&bb->gpio, &bb->gpio_cfg);
        exti_init(&bb->exti, &bb->exti_cfg);
    }
#endif
}