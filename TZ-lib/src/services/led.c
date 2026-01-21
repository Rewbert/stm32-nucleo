
#include "hal/gpio.h"
#include "hal/clock.h"
#include "services/led.h"

/* These are the hard-wired LEDs on the STM32L552ZEQ board */
gpio_t red_led   = { GPIO_PORT_A, 9 };
gpio_t blue_led  = { GPIO_PORT_B, 7 };
gpio_t green_led = { GPIO_PORT_C, 7 };

/**
 * @brief Initialise an LED such that it is ready to be used. The domain variable determines
 * whether the specific GPIO pin can be accessed/controlled by the nonsecure application.
 * 
 */
void init_led(gpio_t gpio, security_domain_t domain) {
    clock_enable_gpio(gpio.port);
    gpio_set_mode(gpio, GPIO_MODE_OUTPUT);

    if(domain == DOMAIN_NONSECURE) {
        gpio_make_nonsecure(gpio);
    }
}