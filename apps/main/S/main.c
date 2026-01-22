
#include "hal/irq.h"
#include "hal/gpio.h"
#include "hal/exti.h"
#include "hal/uart.h"
#include "hal/platform/clock.h"
#include "hal/platform/domain.h"

#include "services/button.h"
#include "services/led.h"
#include "services/uart.h"

void my_button_callback(exti_edge_t edge) {
    uart_write_string(HAL_LPUART1, "hello from the button callback, with the new TZ lib!\r\n");
}

void main(void) {
    platform_clock_configure_110mhz();
    configure_systick(110000);
    enable_lpuart1(244444, DOMAIN_SECURE);
    
    enable_irq();

    init_led(red_led, DOMAIN_SECURE);
    init_led(blue_led, DOMAIN_SECURE);
    init_led(green_led, DOMAIN_SECURE);

    init_button(blue_button, EXTI_EDGE_FALLING, DOMAIN_SECURE);
    register_button_callback(blue_button, &my_button_callback);

    while(1) {
        toggle_gpio(red_led);
        toggle_gpio(blue_led);
        toggle_gpio(green_led);

        uart_write_string(HAL_LPUART1, "hello world from uart_write_string!\r\n");

        delay_ms(500);
    }
}

NONSECURE_CALLABLE int add10(int x) {
    return x + 10;
}