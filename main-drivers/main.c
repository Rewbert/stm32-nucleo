#include "stm32l5xx.h" // only need this for the m33-specific SysTick-Config (for now), and __enable_irq, which I will add my own driver/lib function for
#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "boards/board.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/exti.h"
#include "drivers/rcc.h"
#include "drivers/pwr.h"
#include "drivers/systick.h"

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
        .mode      = GPIO_MODE_AF,
        .pull      = GPIO_PULLUP,
        .alternate = GPIO_AF8,
    };
    gpio_init(&pg7, &uart_pin_cfg);
    gpio_init(&pg8, &uart_pin_cfg);

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);
}

static void sys_init(void) {
    board_init();

    rcc_configure_pll(board_rcc(), board_flash(), 55, 1, 7, 110000000); // 110 MHz on my stm32l5

    rcc_enable(board_rcc(), RCC_GPIOA);
    rcc_enable(board_rcc(), RCC_GPIOB);
    rcc_enable(board_rcc(), RCC_GPIOC);

    console_init();
    
    SysTick_Config(110000);
    __enable_irq();
}

static void button_callback(exti_edge_t edge) {
    (void)edge;
    const char msg[] = "hello from the button callback!\r\n";
    uart_write(board_console(), (const uint8_t *)msg, sizeof(msg) - 1);
}

void main(void) {
    sys_init();

    gpio_config_t led_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_led(BOARD_LED_RED),   &led_cfg);
    gpio_init(board_led(BOARD_LED_BLUE),  &led_cfg);
    gpio_init(board_led(BOARD_LED_GREEN), &led_cfg);

    gpio_config_t btn_cfg = {
        .mode      = GPIO_MODE_INPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_button(BOARD_BUTTON_USER), &btn_cfg);

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

    while (1) {
        gpio_toggle(board_led(BOARD_LED_RED));
        gpio_toggle(board_led(BOARD_LED_BLUE));
        gpio_toggle(board_led(BOARD_LED_GREEN));

        const char msg[] = "hello world!\r\n";
        uart_write(board_console(), (const uint8_t *)msg, sizeof(msg) - 1);

        systick_delay_ms(500);
    }
}

NONSECURE_CALLABLE int add(int x) {
    return x+1;
}