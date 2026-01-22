
#include "hal/platform/domain.h"
#include "hal/uart.h"
#include "hal/gpio.h"
#include "hal/clock.h"
#include "services/uart.h"

gpio_t lpuart1_tx = { GPIO_PORT_G, 7 };
gpio_t lpuart1_rx = { GPIO_PORT_G, 8 };

/**
 * @brief Configure and enable the LPUART1 peripheral, such that reading and writing
 * works. Can be configured to belong to the secure or non-secure world.
 * 
 */
void enable_lpuart1(uint32_t brr, security_domain_t domain) {
    uart_select_clock_source(HAL_LPUART1, SYSCLK);
    uart_enable_power(HAL_LPUART1);
    clock_enable_gpio(GPIO_PORT_G);

    gpio_set_mode(lpuart1_tx, GPIO_MODE_AF);
    gpio_set_mode(lpuart1_rx, GPIO_MODE_AF);
    gpio_set_pupdr(lpuart1_tx, GPIO_PULLUP);
    gpio_set_pupdr(lpuart1_rx, GPIO_PULLUP);
    gpio_set_af(lpuart1_tx, GPIO_AF8);
    gpio_set_af(lpuart1_rx, GPIO_AF8);

    uart_set_brr(HAL_LPUART1, brr);
    uart_activate(HAL_LPUART1);

    if(domain == DOMAIN_SECURE) {
        platform_lpuart1_make_secure();
    }
}