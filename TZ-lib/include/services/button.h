#ifndef SERVICES_BUTTON_H
#define SERVICES_BUTTON_H

#include "hal/platform/domain.h"
#include "hal/gpio.h"

/* The blue button found on the STM32L552ZEQ board */
extern gpio_t blue_button;

/**
 * @brief Initialise a GPIO as a button (switch).
 * 
 * @param gpio The GPIO in question
 * @param edge The edge on which we want to trigger the button
 * @param domain Whether the button belongs to the secure or non-secure world
 */
void init_button(gpio_t gpio, exti_edge_t edge, security_domain_t domain);

void register_button_callback(gpio_t gpio, exti_callback_t);

#endif // SERVICES_BUTTON_H