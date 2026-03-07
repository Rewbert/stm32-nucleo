#include "stm32l5xx.h" // only need this for the m33-specific SysTick_Config (for now)
#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "boards/board.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/exti.h"
#include "drivers/rcc.h"
#include "drivers/pwr.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

static void sys_init(void) {
    board_init();

    rcc_configure_pll(board_rcc(), board_flash(), 55, 1, 7, 110000000); // 110 MHz on my stm32l5

    rcc_enable(board_rcc(), RCC_GPIOA);
    rcc_enable(board_rcc(), RCC_GPIOB);
    rcc_enable(board_rcc(), RCC_GPIOC);

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);
    
    SysTick_Config(110000);
    irq_enable();
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


    board_button_init(board_button(BOARD_BUTTON_USER), GPIO_SECURE, EXTI_EDGE_FALLING, button_callback);

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