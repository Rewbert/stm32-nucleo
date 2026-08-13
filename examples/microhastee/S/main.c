#include "domain/domain.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/rcc.h"
#include "drivers/tzsc.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "firmware/boards/board.h"

#include <string.h>
#include <stdlib.h>
#include <arm_cmse.h>
#include "config.h"

/* Upper bound on a serialised closure/Callable message (function index + all
   argument bytes) that we're willing to copy onto the Secure heap. We expect
   these messages to stay small; this just needs to be comfortably above the
   largest legitimate call while still bounding a NS-controlled allocation. */
#define MAX_NSC_INPUT_LEN 8192

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
    // };
    // gpio_init(board_led(BOARD_LED_RED), &red_cfg);
    // gpio_init(board_led(BOARD_LED_BLUE), &red_cfg);

    // gpio_config_t green_cfg = {
    //     .mode            = GPIO_MODE_OUTPUT,
    //     .pull            = GPIO_NOPULL,
    //     .alternate       = GPIO_AF0,
    // };
    // gpio_init(board_led(BOARD_LED_GREEN), &green_cfg);
    // gpio_set_security(board_led(BOARD_LED_GREEN), GPIO_NONSECURE);

    // irq_enable();
}

void stm32_exit(int n) {
    gpio_toggle(board_led(BOARD_LED_RED));
}

/*** These come from Haskell ***/

extern void c_handle_nsc_call(const uint8_t *in_buf, int in_len,
                               uint8_t *out_buf, int out_capacity, int *out_len);
extern void app_main();

/*******************************/

NONSECURE_CALLABLE void sg(struct BFILE *input_bfile,
                           uint8_t *output_buf, int output_capacity, int *output_len) {
    /*
    We manually fetch the buffer in this hacky way (Please don't change the layout of BFILE, Lennart),
    because CHECKBFILE asserts that a certain function pointer points to a specific function (get_mem()). We
    do use the right one, but we have allocated the BFILE in the NS world, and the check is done by
    the S world, which has its own copy of get_mem(). These function pointers are not equal, and we would thus
    throw an error.

    I bet there is some flag to turn off that disables this check (SANITY?), but the specific flag seems to have
    helped me catch several bugs before, so I'd prefer to leave it 'on'.
    */

    /* output_len is the one out-param we might need before we've validated anything
       else, so it comes first: if NS didn't even give us a writable slot for it, there
       is nothing safe left to do but return. */
    if (!cmse_check_address_range(output_len, sizeof(*output_len), CMSE_NONSECURE | CMSE_MPU_READWRITE)) {
        return;
    }

    struct { void *fn[7]; size_t size; size_t pos; uint8_t *buf; } *p = (void*)input_bfile;

    /* Every pointer below is NS-supplied and must be proven to actually lie in
       non-secure memory before we dereference it -- a malicious or buggy NS caller
       could otherwise point us at secure memory (arbitrary secure-side read via
       p->buf, or arbitrary secure-side write via output_buf). cmse_check_address_range
       is the Armv8-M-mandated way to do that check (TT-instruction based, not a
       software convention). */
    if (!cmse_check_address_range(p, sizeof(*p), CMSE_NONSECURE | CMSE_MPU_READ) ||
        !cmse_check_address_range(p->buf, p->pos, CMSE_NONSECURE | CMSE_MPU_READ) ||
        !cmse_check_address_range(output_buf, output_capacity, CMSE_NONSECURE | CMSE_MPU_READWRITE) ||
        p->pos > MAX_NSC_INPUT_LEN) {
        *output_len = -1;
        return;
    }

    /* p->buf is validated above, but it still lives in NS memory: nothing
       stops the NS side (or NS-attributed DMA) from mutating it while we are
       part-way through deserialising, since that happens over many separate
       reads rather than all at once. Copy it into a Secure-owned buffer here
       and deserialise from that copy instead, so the bytes we parse can't
       change out from under us after this point. */
    uint8_t *secure_copy = malloc(p->pos);
    if (!secure_copy) {
        *output_len = -1;
        return;
    }
    memcpy(secure_copy, p->buf, p->pos);

    c_handle_nsc_call(secure_copy, (int)p->pos, output_buf, output_capacity, output_len);

    free(secure_copy);
}

void main(void) {
    board_init();
    stm32_init();
    mhs_main(0,0);
    app_main();
}
