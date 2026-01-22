
#include "hal/platform/domain.h"
#include "hal/gpio.h"
#include "hal/exti.h"
#include "hal/clock.h"
#include "services/button.h"

gpio_t blue_button = { GPIO_PORT_C, 13 };

void init_button(gpio_t gpio, exti_edge_t edge, security_domain_t domain) {
    clock_enable_gpio(gpio.port);

    gpio_set_mode(gpio, GPIO_MODE_INPUT);
    gpio_set_pupdr(gpio, GPIO_PULLUP);

    exti_line_t exti = {gpio.port, gpio.pin};
    exti_route_pin(exti);
    exti_unmask_interrupts(exti);
    exti_trigger_on(exti, edge);
    
    if(domain == DOMAIN_SECURE) {
        gpio_make_secure(gpio);
        exti_make_secure(exti);
    } else {
        gpio_make_nonsecure(gpio);
        exti_NVIC_set_target_state(exti);
    }

    exti_configure_NVIC(exti, 2);
}

void register_button_callback(gpio_t gpio, exti_callback_t cb) {
    platform_exti_register_callback( (exti_line_t) { gpio.port, (uint8_t) gpio.pin }, cb);
}