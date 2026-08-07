#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "firmware/boards/board.h"

#include "drivers/gpio.h"
#include "drivers/rcc.h"
#include "drivers/tzsc.h"
#include "drivers/nvic.h"
#include "drivers/timer.h"

/* Placeholder NSC function — gives the linker something to put in the import lib. */
int __attribute__((cmse_nonsecure_entry)) nsc_placeholder(void) { return 0; }

/*
 * Secure world only brings the board up, releases the green LED and TIM6 to
 * non-secure, and hands off. See NS/main.c for the actual 1 Hz blink.
 *
 * Clock-gating (rcc_enable) only works from secure code in this driver set,
 * so TIM6's clock has to be turned on here even though non-secure is the one
 * that will drive it. NVIC configuration for a non-secure-targeted IRQ is
 * likewise done from secure code — see examples/breadboard-buttons for the
 * same pattern applied to EXTI.
 */
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
    gpio_set_security(board_led(BOARD_LED_GREEN), GPIO_NONSECURE);

    tzsc_set_periph(board_tzsc(), TZSC_PERIPH_TIM6, TZSC_NONSECURE);

    int irqn = timer_irqn(board_timer());
    nvic_set_priority(irqn, 0);
    nvic_set_target_nonsecure(irqn);
    nvic_enable_irq(irqn);

    /* Falling off the end of main() hands off to non-secure — see tz_init.c. */
}
