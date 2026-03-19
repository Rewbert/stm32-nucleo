/*
 * Board: STM32U5A5ZJQ Nucleo-144 (MB1549)
 *
 * Pins (per UM2861):
 *   Green LED : PC7
 *   Blue  LED : PB7
 *   Red   LED : PG2
 *
 *   User button: PC13 (active low)
 *
 *   Console UART: USART1 (PA9=TX, PA10=RX, AF7) — default VCP wiring
 *   GPIOG requires VDDIO2 (for red LED on PG2) — enabled via PWR_SVMCR.IO2SV (U5)
 */

#include "boards/board.h"

#include "domain/cmsis_select.h"
#include "stm32u5xx.h"

#include "backends/stm32u5/gpio.h"
#include "backends/stm32u5/exti.h"
#include "backends/stm32u5/usart1.h"
#include "backends/stm32u5/flash.h"
#include "backends/stm32u5/rcc.h"
#include "backends/stm32u5/pwr.h"
#include "backends/stm32u5/mpcbb.h"
#include "backends/stm32u5/tzsc.h"

_Static_assert(sizeof(board_gpio_backend_t) >= sizeof(stm32u5_gpio_backend_t),
               "BOARD_GPIO_BACKEND_SIZE is too small for stm32u5_gpio_backend_t");

void board_gpio_create(gpio_dev_t *dev, board_gpio_port_t port, uint8_t pin, board_gpio_backend_t *backend) {
    static GPIO_TypeDef * const port_map[] = {
        [BOARD_GPIO_PORT_A] = GPIO_CMSIS(A),
        [BOARD_GPIO_PORT_B] = GPIO_CMSIS(B),
        [BOARD_GPIO_PORT_C] = GPIO_CMSIS(C),
        [BOARD_GPIO_PORT_D] = GPIO_CMSIS(D),
        [BOARD_GPIO_PORT_E] = GPIO_CMSIS(E),
        [BOARD_GPIO_PORT_F] = GPIO_CMSIS(F),
        [BOARD_GPIO_PORT_G] = GPIO_CMSIS(G),
        [BOARD_GPIO_PORT_H] = GPIO_CMSIS(H),
    };
    stm32u5_gpio_create(dev, port_map[port], pin, (stm32u5_gpio_backend_t *)backend->_opaque);
}

_Static_assert(sizeof(board_exti_backend_t) >= sizeof(stm32u5_exti_backend_t),
               "BOARD_EXTI_BACKEND_SIZE is too small for stm32u5_exti_backend_t");

void board_exti_create(exti_dev_t *dev, board_exti_backend_t *backend, uint8_t pin) {
    stm32u5_exti_create(dev, (stm32u5_exti_backend_t *)backend->_opaque, pin);
}

static stm32u5_gpio_backend_t led_backends[3];
static gpio_dev_t             leds[3];

static stm32u5_gpio_backend_t button_gpio_backend;
static gpio_dev_t             button_gpio;

static stm32u5_exti_backend_t button_exti_backend;
static exti_dev_t             button_exti;

static stm32u5_usart1_backend_t console_backend;
static uart_dev_t               console;

static rcc_dev_t rcc;
static pwr_dev_t pwr;

static stm32u5_flash_backend_t flash_backend;
static flash_dev_t             flash;

static stm32u5_tzsc_backend_t tzsc_backend;
static tzsc_dev_t             tzsc;

static inline void console_init(void);

void board_init(void) {
    /* Red LED is on PG2, which sits on the VDDIO2 rail — enable it before
     * touching GPIOG. Green (PC7) and blue (PB7) are on the main supply. */
    stm32u5_rcc_create(&rcc);
    stm32u5_pwr_create(&pwr);
    rcc_enable(&rcc, RCC_PWR);
    pwr_enable_vddio2(&pwr);
    rcc_enable(&rcc, RCC_GPIOG);

    stm32u5_gpio_create(&leds[BOARD_LED_GREEN], GPIO_CMSIS(C), 7, &led_backends[BOARD_LED_GREEN]);
    stm32u5_gpio_create(&leds[BOARD_LED_BLUE],  GPIO_CMSIS(B), 7, &led_backends[BOARD_LED_BLUE]);
    stm32u5_gpio_create(&leds[BOARD_LED_RED],   GPIO_CMSIS(G), 2, &led_backends[BOARD_LED_RED]);

    stm32u5_gpio_create(&button_gpio, GPIO_CMSIS(C), 13, &button_gpio_backend);
    stm32u5_exti_create(&button_exti, &button_exti_backend, 13); /* PC13 user button */

    stm32u5_usart1_create(&console, USART1x, &console_backend);

    stm32u5_flash_create(&flash, FLASHx, &flash_backend);
    stm32u5_tzsc_create(&tzsc, &tzsc_backend);

    console_init();
}

static inline void console_init(void) {
    /* USART1 is on PA9 (TX) / PA10 (RX), AF7 — default VCP wiring on MB1549.
     * GPIOA is on the main supply; no VDDIO2 needed here. */
    rcc_enable(board_rcc(), RCC_GPIOA);
    rcc_enable(board_rcc(), RCC_USART1);
    rcc_set_peripheral_clock(board_rcc(), RCC_USART1, RCC_SYSCLK);

    board_gpio_backend_t pa9_backend, pa10_backend;
    gpio_dev_t pa9, pa10;
    board_gpio_create(&pa9,  BOARD_GPIO_PORT_A, 9,  &pa9_backend);
    board_gpio_create(&pa10, BOARD_GPIO_PORT_A, 10, &pa10_backend);
    gpio_config_t uart_pin_cfg = {
        .mode            = GPIO_MODE_AF,
        .pull            = GPIO_PULLUP,
        .alternate       = GPIO_AF7,
        .security_domain = GPIO_SECURE,
    };
    gpio_init(&pa9,  &uart_pin_cfg);
    gpio_init(&pa10, &uart_pin_cfg);
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
        .port             = EXTI_PORT_C,
        .pin              = 13,
        .edge             = EXTI_EDGE_FALLING,
        .priority         = 0,
        .secure           = true,
        .target_nonsecure = false,
    };
    exti_init(board_button_exti(BOARD_BUTTON_USER), &exti_cfg);
    exti_register_callback(board_button_exti(BOARD_BUTTON_USER), button_callback);
}

void board_configure_pll(void) {
    /* STM32U5A5ZJQ: MSIS 4 MHz × 40 / 1 / 1 → 160 MHz.
     * VOS + EPOD booster sequencing is handled inside rcc_configure_pll. */
    rcc_configure_pll(board_rcc(), board_pwr(), board_flash(), 40, 1, 1, 160000000);
}

uint32_t board_sysclk_hz(void) {
    return 160000000U;
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

tzsc_periph_t board_console_periph(void) {
    return TZSC_PERIPH_USART1;
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

/* STM32U5A5ZJQ SRAM layout (GTZC1 controls MPCBB1-3/5, GTZC2 controls MPCBB4):
 *   MPCBB1 → SRAM1  768 KB  @ 0x20000000  (48 superblocks × 16 KB)
 *   MPCBB2 → SRAM2   64 KB  @ 0x200C0000  ( 4 superblocks × 16 KB)
 *   MPCBB3 → SRAM3  512 KB  @ 0x200D0000  (32 superblocks × 16 KB)
 *   MPCBB4 → SRAM4   16 KB  @ 0x28000000  ( 1 superblock  × 16 KB)
 *   MPCBB5 → SRAM5  768 KB  @ 0x200E0000  (48 superblocks × 16 KB)
 */
#define U5_MPCBB_COUNT 5

static stm32u5_mpcbb_backend_t mpcbb_backends[U5_MPCBB_COUNT];
static mpcbb_dev_t             mpcbbs[U5_MPCBB_COUNT];

#if HAL_SECURE
static uint8_t gtzc1_enabled;
static uint8_t gtzc2_enabled;
static uint8_t mpcbb_created[U5_MPCBB_COUNT];

static GTZC_MPCBB_TypeDef * const mpcbb_regs[U5_MPCBB_COUNT] = {
    GTZC_MPCBB1_S, /* index 0 = SRAM1 */
    GTZC_MPCBB2_S, /* index 1 = SRAM2 */
    GTZC_MPCBB3_S, /* index 2 = SRAM3 */
    GTZC_MPCBB4_S, /* index 3 = SRAM4 (under GTZC2) */
    GTZC_MPCBB5_S, /* index 4 = SRAM5 */
};
#endif

mpcbb_dev_t *board_mpcbb(int index) {
#if HAL_SECURE
    if (index < 0 || index >= U5_MPCBB_COUNT) return NULL;

    /* MPCBB4 (index 3) is under GTZC2; all others are under GTZC1. */
    if (index == 3) {
        if (!gtzc2_enabled) {
            rcc_dev_t tmp_rcc;
            stm32u5_rcc_create(&tmp_rcc);
            rcc_enable(&tmp_rcc, RCC_GTZC2);
            gtzc2_enabled = 1;
        }
    } else {
        if (!gtzc1_enabled) {
            rcc_dev_t tmp_rcc;
            stm32u5_rcc_create(&tmp_rcc);
            rcc_enable(&tmp_rcc, RCC_GTZC);
            gtzc1_enabled = 1;
        }
    }

    if (!mpcbb_created[index]) {
        stm32u5_mpcbb_create(&mpcbbs[index], mpcbb_regs[index], &mpcbb_backends[index]);
        mpcbb_created[index] = 1;
    }

    return &mpcbbs[index];
#endif
    return NULL;
}
