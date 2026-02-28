#ifndef DRIVERS_SYSTICK_H
#define DRIVERS_SYSTICK_H

#include <stdint.h>

/*
 * SysTick driver.
 *
 * SysTick belongs to the CPU itself, in this case an Arm Cortex-M33. The handler
 * is fetched from the vector table, so the bootloader must provide it.
 *
 * When you use TrustZone, the secure side must call systick_configure. The nonsecure
 * side still requires its own entry in the vector table, however.
 *
 */

/*
 * Configure SysTick to fire every `reload` core clock cycles.
 *
 * In a secure build, also configures the non-secure SysTick with the same
 * period. In a non-secure build, only the local SysTick is configured.
 *
 * For a 1 ms tick at 110 MHz, pass 110000.
 */
void systick_configure(uint32_t reload);

/*
 * Returns the current tick count
 */
uint32_t systick_get_ticks(void);

/*
 * Busy-wait delay. Halts until `ms` ticks have elapsed.
 * Assumes systick_configure() was called with a 1 ms period.
 *
 * If not, I make no guarantees about behavior.
 */
void systick_delay_ms(uint32_t ms);

/*
 * This function is, if existing, called from the systick handler. It allows you to
 * add your own functionality to the CPU systick machinery.
 *
 * It is a weak alias, meaning that if you just declare a function of this same
 * signature, it will be used in place of the weak one.
 */
void systick_callback(uint32_t ticks);

/* This must be placed in the vector table by the bootloader. Give the prototype in the
 * bootloader and mark it as extern.
 *
 * NOTE: You, the programmer, are not supposed to invoke this or do anything to is.
 */
void systick_handler(void);

#endif // DRIVERS_SYSTICK_H
