#include "firmware/boards/board.h"

#include "drivers/gpio.h"
#include "drivers/timer.h"
#include "drivers/irq.h"

/*
 * Blinks the green LED at 1 Hz using TIM6 — both released to us by the
 * secure world in S/main.c. NVIC configuration for this IRQ was also done
 * from secure code, so we only need to bring up the timer itself and unmask
 * interrupts for our own execution state.
 *
 * TIM6 runs one-pulse, so every expiry re-arms itself for the next
 * half-period and toggles the LED. A 1 Hz blink is two toggles a second, so
 * the timer needs to fire every 500 ms.
 */

#define TIMER_TICK_HZ  10000U               /* 0.1 ms per tick */
#define BLINK_TICKS    (TIMER_TICK_HZ / 2U) /* 500 ms */

static void blink_callback(void) {
    gpio_toggle(board_led(BOARD_LED_GREEN));
    timer_start(board_timer(), BLINK_TICKS);
}

void main(void) {
    board_init();

    timer_config_t timer_cfg = {
        .prescaler = (uint16_t)((board_sysclk_hz() / TIMER_TICK_HZ) - 1U),
    };
    timer_init(board_timer(), &timer_cfg);
    timer_register_callback(board_timer(), blink_callback);

    irq_enable();

    timer_start(board_timer(), BLINK_TICKS);

    while (1) {
    }
}
