
#include "hal/irq.h"
#include "hal/gpio.h"
#include "hal/exti.h"
#include "hal/uart.h"
#include "hal/platform/flash.h"
#include "hal/platform/clock.h"
#include "hal/platform/domain.h"

#include "services/button.h"
#include "services/led.h"
#include "services/uart.h"

#include "shared.h"

/**
 * @brief This variable indicates whether the pin has been changed or not. If the value is
 * 0xFFFFFFFF, the pin has never been changed.
 * 
 */
PERSISTENT uint32_t def;

/**
 * @brief The pin code, stored in secure FLASH for persistency.
 * 
 */
PERSISTENT uint32_t pin[4];

/**
 * @brief Check whether the pin has never been changed.
 * 
 */
int isDefaultConfigured() {
    return def == 0xFFFFFFFF;
}

/**
 * @brief Verify that an entered candidate pin is equal to the actual pin.
 * 
 * NOTE: This function exists solely in the secure world. The non-secure world cannot repeatedly
 * invoke this method.
 */
int verifyPin(uint32_t *candidate) {
    for(int i = 0; i < 4; i++) {
        if (!(pin[i] == candidate[i])) {
            return 0;
        }
    }
    return 1;
}

/**
 * @brief Simulate locking the door.
 *
 * NOTE: This is performing a privileged action, and cannot immediately be invoked from the
 * non-secure world.
 * 
 */
void lock() {
    set_gpio(red_led, HIGH);
    set_gpio(green_led, LOW);
}

/**
 * @brief Simulate unlocking the door.
 *
 * NOTE: This is performing a privileged action, and cannot immediately be invoked from the
 * non-secure world.
 * 
 */
void unlock() {
    set_gpio(red_led, LOW);
    set_gpio(green_led, HIGH);
}

/**
 * @brief Given the correct pin, lock the door.
 *
 * NOTE: This is a non-secure callable function, meaning that the non-secure application
 * can invoke it. The privileged operation (locking the door) is guarded by the pin.
 * 
 */
NONSECURE_CALLABLE int lockDoor(uint32_t *code) {
    if(verifyPin(code)) {
        lock();
        return 1;
    }
    return 0;
}

/**
 * @brief Given the correct pin, unlock the door.
 *
 * NOTE: This is a non-secure callable function, meaning that the non-secure application
 * can invoke it. The privileged operation (unlocking the door) is guarded by the pin.
 * 
 */
NONSECURE_CALLABLE int unlockDoor(uint32_t *code) {
    if(verifyPin(code)) {
        unlock();
        return 1;
    }
    return 0;
}

/**
 * @brief Given the correct pin, reprogram the application to use a new pin.
 * 
 * NOTE: This is a non-secure callable function, meaning that the non-secure application
 * can invoke it. It is doing a lot of privileged operations, such as rewriting FLASH. All
 * of these operations are guarded by first supplying the correct pin code.
 */
NONSECURE_CALLABLE int changePin(uint32_t *old, uint32_t *new) {
    if(verifyPin(old)) {
        disable_irq();
        if(!flash_secure_erase_page(1, 125)) return 0;
        if(!flash_secure_program_dw((uint32_t)&pin[0], ((uint64_t)new[1] << 32 | new[0]))) return 0;
        if(!flash_secure_program_dw((uint32_t)&pin[2], ((uint64_t)new[3] << 32 | new[2]))) return 0;
        if(def) {
            if(!flash_secure_program_dw((uint32_t)&def, 0x00000000)) return 0;
        }
        enable_irq();
        return 1;
    }
    return 0;
}

void main(void) {
    platform_clock_configure_110mhz();
    configure_systick(110000);
    enable_lpuart1(244444, DOMAIN_NONSECURE);

    enable_irq();

    init_button(a2, EXTI_EDGE_FALLING, DOMAIN_NONSECURE);
    init_button(a3, EXTI_EDGE_FALLING, DOMAIN_NONSECURE);
    init_button(a5, EXTI_EDGE_FALLING, DOMAIN_NONSECURE);
    init_button(a6, EXTI_EDGE_FALLING, DOMAIN_NONSECURE);
    init_button(a7, EXTI_EDGE_FALLING, DOMAIN_NONSECURE);
    init_button(a8, EXTI_EDGE_FALLING, DOMAIN_NONSECURE);
    init_button(a10, EXTI_EDGE_FALLING, DOMAIN_NONSECURE);

    init_led(red_led, DOMAIN_SECURE);
    init_led(green_led, DOMAIN_SECURE);
}