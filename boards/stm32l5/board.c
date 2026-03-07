/*
 * Board: STM32L552ZEQ Nucleo-144
 *
 * Pins:

 *   green LED : PC7
 *   blue  LED : PB7
 *   red   LED : PA9
 *
 *   The blue button: PC13 (active low)
 *
 *   UART : LPUART1 (PG7=TX, PG8=RX)
 */

#include "boards/board.h"

#include "domain/cmsis_select.h"
#include "stm32l5xx.h"

#include "backends/stm32l5/gpio.h"
#include "backends/stm32l5/exti.h"
#include "backends/stm32l5/lpuart1.h"
#include "backends/stm32l5/flash.h"
#include "backends/stm32l5/rcc.h"
#include "backends/stm32l5/pwr.h"
#include "backends/stm32l5/mpcbb.h"
#include "backends/stm32l5/tzsc.h"

/**
 * @brief I saw this online, a C11 macro to do compiletime checks. If the backend stm32l5 gpio type get's too large
 * the check will fail already at compile time!
 */
_Static_assert(sizeof(board_gpio_backend_t) >= sizeof(stm32l5_gpio_backend_t), "BOARD_GPIO_BACKEND_SIZE is too small for stm32l5_gpio_backend_t");

void board_gpio_create(gpio_dev_t *dev, board_gpio_port_t port, uint8_t pin, board_gpio_backend_t *backend) {
    static GPIO_TypeDef * const port_map[] = { [BOARD_GPIO_PORT_A] = GPIO_CMSIS(A),
                                               [BOARD_GPIO_PORT_B] = GPIO_CMSIS(B),
                                               [BOARD_GPIO_PORT_C] = GPIO_CMSIS(C),
                                               [BOARD_GPIO_PORT_D] = GPIO_CMSIS(D),
                                               [BOARD_GPIO_PORT_E] = GPIO_CMSIS(E),
                                               [BOARD_GPIO_PORT_F] = GPIO_CMSIS(F),
                                               [BOARD_GPIO_PORT_G] = GPIO_CMSIS(G),
                                               [BOARD_GPIO_PORT_H] = GPIO_CMSIS(H),
                                             };
    stm32l5_gpio_create(dev, port_map[port], pin, (stm32l5_gpio_backend_t *)backend->_opaque);
}

_Static_assert(sizeof(board_exti_backend_t) >= sizeof(stm32l5_exti_backend_t), "BOARD_EXTI_BACKEND_SIZE is too small for stm32l5_exti_backend_t");

void board_exti_create(exti_dev_t *dev, board_exti_backend_t *backend) {
    stm32l5_exti_create(dev, (stm32l5_exti_backend_t *)backend->_opaque);
}

static stm32l5_gpio_backend_t led_backends[3];
static gpio_dev_t             leds[3];

static stm32l5_gpio_backend_t button_gpio_backend;
static gpio_dev_t             button_gpio;

static stm32l5_exti_backend_t button_exti_backend;
static exti_dev_t             button_exti;

static stm32l5_lpuart1_backend_t console_backend;
static uart_dev_t                console;

static rcc_dev_t   rcc;
static pwr_dev_t   pwr;

static stm32l5_flash_backend_t flash_backend;
static flash_dev_t             flash;

static stm32l5_tzsc_backend_t tzsc_backend;
static tzsc_dev_t             tzsc;

static inline void console_init();

void board_init(void) {
    stm32l5_gpio_create(&leds[BOARD_LED_GREEN], GPIO_CMSIS(C), 7,  &led_backends[BOARD_LED_GREEN]);
    stm32l5_gpio_create(&leds[BOARD_LED_BLUE],  GPIO_CMSIS(B), 7,  &led_backends[BOARD_LED_BLUE]);
    stm32l5_gpio_create(&leds[BOARD_LED_RED],   GPIO_CMSIS(A), 9,  &led_backends[BOARD_LED_RED]);

    stm32l5_gpio_create(&button_gpio, GPIO_CMSIS(C), 13, &button_gpio_backend);
    stm32l5_exti_create(&button_exti, &button_exti_backend);

    stm32l5_lpuart1_create(&console, LPUART1x, &console_backend);

    stm32l5_rcc_create(&rcc);
    stm32l5_pwr_create(&pwr);
    stm32l5_flash_create(&flash, FLASHx, &flash_backend);
    stm32l5_tzsc_create(&tzsc, &tzsc_backend);

    console_init();
}

static inline void console_init() {
    // GPIOG (and as a result LPUART1 on PG7/PG8) requires VDDIO2
    rcc_enable(board_rcc(), RCC_PWR);
    pwr_enable_vddio2(board_pwr());

    rcc_enable(board_rcc(), RCC_GPIOG);

    rcc_enable(board_rcc(), RCC_LPUART1);
    rcc_set_peripheral_clock(board_rcc(), RCC_LPUART1, RCC_SYSCLK);

    board_gpio_backend_t pg7_backend, pg8_backend;
    gpio_dev_t pg7, pg8;
    board_gpio_create(&pg7, BOARD_GPIO_PORT_G, 7, &pg7_backend);
    board_gpio_create(&pg8, BOARD_GPIO_PORT_G, 8, &pg8_backend);
    gpio_config_t uart_pin_cfg = {
        .mode            = GPIO_MODE_AF,
        .pull            = GPIO_PULLUP,
        .alternate       = GPIO_AF8,
        .security_domain = GPIO_SECURE,
    };
    gpio_init(&pg7, &uart_pin_cfg);
    gpio_init(&pg8, &uart_pin_cfg);
}

void board_button_init(gpio_dev_t *button, gpio_security_t security, exti_edge_t edge, void (*button_callback)(exti_edge_t edge)) {
    gpio_config_t btn_cfg = {
        .mode            = GPIO_MODE_INPUT,
        .pull            = GPIO_NOPULL,
        .alternate       = GPIO_AF0,
        .security_domain = security,
    };
    gpio_init(button, &btn_cfg);

    exti_config_t exti_cfg = {
        .port             = EXTI_PORT_C, // this stuff is duplicated. I already wrote this stuff in board_init. Need to
        .pin              = 13,          // think about how to remove the deplication
        .edge             = EXTI_EDGE_FALLING,
        .priority         = 0,
        .secure           = true,
        .target_nonsecure = false,
    };
    exti_init(board_button_exti(BOARD_BUTTON_USER), &exti_cfg);
    exti_register_callback(board_button_exti(BOARD_BUTTON_USER), button_callback);
}

gpio_dev_t *board_led(board_led_t led) {
    return &leds[led];
}

gpio_dev_t *board_button(board_button_t btn) {
    (void)btn;
    return &button_gpio;
}

exti_dev_t *board_button_exti(board_button_t btn) {
    (void)btn;
    return &button_exti;
}

uart_dev_t *board_console(void) {
    return &console;
}

rcc_dev_t *board_rcc(void) {
    return &rcc;
}

flash_dev_t *board_flash(void) {
    return &flash;
}

pwr_dev_t *board_pwr(void) {
    return &pwr;
}

tzsc_dev_t *board_tzsc(void) {
    return &tzsc;
}

static stm32l5_mpcbb_backend_t mpcbb1_backend;
static mpcbb_dev_t mpcbb1;

static stm32l5_mpcbb_backend_t mpcbb2_backend;
static mpcbb_dev_t mpcbb2;

#if HAL_SECURE
uint8_t gtzc_enabled;
uint8_t mpcbb1_created;
uint8_t mpcbb2_created;
#endif

mpcbb_dev_t *board_mpcbb(int index) {
#if HAL_SECURE
    if(index >= BOARD_MPCBB_COUNT) return NULL;

    if(!gtzc_enabled) {
        rcc_dev_t rcc;
        stm32l5_rcc_create(&rcc);
        rcc_enable(&rcc, RCC_GTZC);
        gtzc_enabled = 1;
    }

    switch(index) {
        case 0: {
            if(!mpcbb1_created) {
                stm32l5_mpcbb_create(&mpcbb1, GTZC_MPCBB1, &mpcbb1_backend);
                mpcbb1_created = 1;
            }
            return &mpcbb1;
        }
        case 1: {
            if(!mpcbb2_created) {
                stm32l5_mpcbb_create(&mpcbb2, GTZC_MPCBB2, &mpcbb2_backend);
                mpcbb2_created = 1;
            }
            return &mpcbb2;
        }
        default: return NULL;
    }
#endif
    return NULL;
}