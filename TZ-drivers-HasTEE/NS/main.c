#include "domain/domain.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "boards/board.h"

#include "sg.h"

int mhs_main(int argc, char **argv);

/* SysTick only — PLL and UART were configured by the secure world and must not be touched again. */
void stm32_init() {
    systick_configure(board_sysclk_hz() / 1000);
    irq_enable();
}

void stm32_exit(int n) {
    gpio_toggle(board_led(BOARD_LED_GREEN));
}

void main(void) {
    board_init();
    stm32_init();
    mhs_main(0, 0);
}
