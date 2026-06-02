#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/exti.h"
#include "drivers/rcc.h"
#include "drivers/pwr.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "boards/board.h"
#include "breadboard.h"

/**
 * @brief This is from the secure world.
 * 
 */
extern void secure_print(const uint8_t *str, const uint8_t len);

// since my example callbacks are all the same, I define a macro for them to save some space
#define A_CALLBACK(pin) \
static void a##pin##_callback(exti_edge_t edge) {        \
    (void)edge;                                          \
    const char msg[] = "a" #pin "\r\n";                    \
    secure_print((const uint8_t *)msg, sizeof(msg) - 1); \
}

A_CALLBACK(2)
A_CALLBACK(3)
A_CALLBACK(5)
A_CALLBACK(6)
A_CALLBACK(7)
A_CALLBACK(8)
A_CALLBACK(10)

void main(void) {
    board_init();
    breadboard_init();

    // install exti callbacks
    exti_register_callback(breadboard_get_exti(A2),  a2_callback);
    exti_register_callback(breadboard_get_exti(A3),  a3_callback);
    exti_register_callback(breadboard_get_exti(A5),  a5_callback);
    exti_register_callback(breadboard_get_exti(A6),  a6_callback);
    exti_register_callback(breadboard_get_exti(A7),  a7_callback);
    exti_register_callback(breadboard_get_exti(A8),  a8_callback);
    exti_register_callback(breadboard_get_exti(A10), a10_callback);

    irq_enable();

    // nonsecure app

    while(1) {

    }
}