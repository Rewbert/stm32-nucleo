#include "domain/domain.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/rcc.h"
#include "drivers/tzsc.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "boards/board.h"

#include <string.h>
#include "config.h"

int mhs_main(int argc, char **argv);

/* Configure clocks, SysTick, UART (assigned to nonsecure), and LEDs. */
void stm32_init() {
    // board_configure_pll();
    // systick_configure(board_sysclk_hz() / 1000);

    // /* Release UART to the nonsecure world, then configure it. */
    // tzsc_set_periph(board_tzsc(), board_console_periph(), TZSC_NONSECURE);

    // uart_config_t uart_cfg = {
    //     .baudrate    = 115200,
    //     .word_length = 8,
    //     .stop_bits   = 1,
    //     .parity      = UART_PARITY_NONE,
    // };
    // uart_init(board_console(), &uart_cfg);

    // /* Enable GPIO clocks and configure LEDs. */
    // rcc_enable(board_rcc(), RCC_GPIOA);
    // rcc_enable(board_rcc(), RCC_GPIOC);
    // rcc_enable(board_rcc(), RCC_GPIOB);

    // gpio_config_t red_cfg = {
    //     .mode            = GPIO_MODE_OUTPUT,
    //     .pull            = GPIO_NOPULL,
    //     .alternate       = GPIO_AF0,
    //     .security_domain = GPIO_SECURE,
    // };
    // gpio_init(board_led(BOARD_LED_RED), &red_cfg);
    // gpio_init(board_led(BOARD_LED_BLUE), &red_cfg);

    // gpio_config_t green_cfg = {
    //     .mode            = GPIO_MODE_OUTPUT,
    //     .pull            = GPIO_NOPULL,
    //     .alternate       = GPIO_AF0,
    //     .security_domain = GPIO_NONSECURE,
    // };
    // gpio_init(board_led(BOARD_LED_GREEN), &green_cfg);

    // irq_enable();
}

void stm32_exit(int n) {
    gpio_toggle(board_led(BOARD_LED_RED));
}

/*** These come from Haskell ***/

extern void c_handle_nsc_call(const uint8_t *in_buf, int in_len,
                               uint8_t *out_buf, int *out_len);
extern void app_main();

/*******************************/

NONSECURE_CALLABLE void sg(struct BFILE *input_bfile,
                           uint8_t *output_buf, int *output_len) {
    /*
    We manually fetch the buffer in this hacky way (Please don't change the layout of BFILE, Lennart),
    because CHECKBFILE asserts that a certain function pointer points to a specific function (get_mem()). We
    do use the right one, but we have allocated the BFILE in the NS world, and the check is done by
    the S world, which has its own copy of get_mem(). These function pointers are not equal, and we would thus
    throw an error.
    
    I bet there is some flag to turn off that disables this check (SANITY?), but the specific flag seems to have
    helped me catch several bugs before, so I'd prefer to leave it 'on'.
    */
    struct { void *fn[7]; size_t size; size_t pos; uint8_t *buf; } *p = (void*)input_bfile;
    c_handle_nsc_call(p->buf, (int)p->pos, output_buf, output_len);
}

void main(void) {
    board_init();
    stm32_init();
    mhs_main(0,0);
    app_main();
}