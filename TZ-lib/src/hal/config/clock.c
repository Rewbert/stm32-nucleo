
#include "hal/platform/domain.h"
#include "hal/platform/cmsis_select.h"
#include "hal/core/gpio.h"
#include "hal/core/clock.h"
#include "hal/config/clock.h"
#include "hal/platform/clock.h"

/**
 * @brief Enable the clock for a specific GPIO port.
 */
void clock_enable_gpio(gpio_port_t port) {
    platform_clock_enable_gpio(port);
}