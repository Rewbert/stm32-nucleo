#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "firmware/boards/board.h"

#include "drivers/gpio.h"
#include "drivers/rcc.h"
#include "drivers/tzsc.h"
#include "drivers/nvic.h"
#include "drivers/timer.h"
#include "drivers/irq.h"

/*
 * Blinks the green LED at 1 Hz using TIM6, entirely within the secure world —
 * no TrustZone hand-off. See examples/timer-blink-nonsecure for the same demo
 * with the timer and LED released to the non-secure world instead.
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

/* Placeholder NSC function — gives the linker something to put in the import lib. */
int __attribute__((cmse_nonsecure_entry)) nsc_placeholder(void) { return 0; }

void main(void) {
    board_init();
    board_configure_pll();

    rcc_enable(board_rcc(), RCC_GPIOC); /* green LED is PC7 */
    rcc_enable(board_rcc(), RCC_TIM6);

    gpio_config_t led_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_led(BOARD_LED_GREEN), &led_cfg);
    gpio_set_security(board_led(BOARD_LED_GREEN), GPIO_SECURE);

    tzsc_set_periph(board_tzsc(), TZSC_PERIPH_TIM6, TZSC_SECURE);

    timer_config_t timer_cfg = {
        .prescaler = (uint16_t)((board_sysclk_hz() / TIMER_TICK_HZ) - 1U),
    };
    timer_init(board_timer(), &timer_cfg);
    timer_register_callback(board_timer(), blink_callback);

    int irqn = timer_irqn(board_timer());
    nvic_set_priority(irqn, 0);
    nvic_enable_irq(irqn);

    irq_enable();

    timer_start(board_timer(), BLINK_TICKS);

    /* The whole application lives here; non-secure never gets to run. */
    while (1) {
    }
}
