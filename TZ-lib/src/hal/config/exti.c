
#include "hal/core/exti.h"
#include "hal/config/exti.h"
#include "hal/platform/exti.h"

/**
 * @brief Make an EXTI line secure, making only the trusted application able to use or configure it.
 * 
 */
void exti_make_secure(exti_line_t exti) {
    platform_exti_make_secure(exti.pin);
}

/**
 * @brief Configure which edge transition an EXTI interrupt will trigger on. Valid alternatives
 * are falling, rising, or both.
 * 
 */
void exti_trigger_on(exti_line_t exti, exti_edge_t edge) {
    platform_exti_trigger_on(exti, edge);
}

/**
 * @brief Unmask interrupt requests for a specific EXTI pin.
 * 
 */
void exti_unmask_interrupts(exti_line_t exti) {
    platform_exti_unmask_interrupts(exti.pin);
}

/**
 * @brief Route a pin to the EXTI peripheral, enabling interrupts to occur.
 * 
 */
void exti_route_pin(exti_line_t exti) {
    platform_exti_route_pin(exti);
}

/**
 * @brief Configure the NVIC (TODO: document this more later)
 * 
 */
void exti_configure_NVIC(exti_line_t exti, int priority) {
    platform_exti_configure_NVIC(exti, priority);
}

/**
 * @brief Set the target state of NVIC
 * 
 */
void exti_NVIC_set_target_state(exti_line_t exti) {
    platform_exti_NVIC_set_target_state(exti);
}