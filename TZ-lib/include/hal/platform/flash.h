#ifndef PLATFORM_FLASH_H
#define PLATFORM_FLASH_H

#include "stm32l5xx.h"

// #include <stdint.h>
#include "hal/platform/domain.h"
#include "hal/platform/cmsis_select.h"
#include "stm32l5xx.h"

/**
 * @brief Unlock the FLASH so that it can be written to.
 * 
 */
static inline void platform_flash_secure_unlock(void) {
#if HAL_SECURE
    // The manual says that in order to unlock the FLASH, we must write these two keys to this
    // specific register in this order.
    if (FLASHx->SECCR & FLASH_SECCR_SECLOCK) {
        FLASHx->SECKEYR = 0x45670123;
        FLASHx->SECKEYR = 0xCDEF89AB;
    }
#endif
}

/**
 * @brief Lock the FLASH, disabling further writes.
 * 
 */
static inline void platform_flash_secure_lock(void) {
#if HAL_SECURE
    if((!(FLASH_S->SECCR & FLASH_SECCR_SECLOCK))) {
        FLASHx->SECCR |= (1 << FLASH_SECCR_SECLOCK_Pos);
    }
#endif
}

/**
 * @brief Wait until the FLASH indicates that it is not busy anymore.
 * 
 */
static inline void platform_flash_wait_busy(void) {
#if HAL_SECURE
    // When FLASH is busy, it will set this bit. This while-loop will not terminate
    // until the bit is un-set.
    while (FLASHx->SECSR & FLASH_SECSR_SECBSY) {}
#endif
}

/**
 * @brief Clear the FLASH flags associated with reading and writing from secure world.
 * 
 */
static inline void platform_flash_clear_secure_flags(void) {
#if HAL_SECURE
    FLASHx->SECSR =
        FLASH_SECSR_SECEOP     |
        FLASH_SECSR_SECOPERR   |
        FLASH_SECSR_SECPROGERR |
        FLASH_SECSR_SECSIZERR  |
        FLASH_SECSR_SECPGAERR  |
        FLASH_SECSR_SECPGSERR  |
        FLASH_SECSR_SECWRPERR;
#endif
}

/**
 * @brief Erase a page of the FLASH memory, identified by the memory bank and page.
 * 
 */
int flash_secure_erase_page(uint32_t bank, uint32_t page);

/**
 * @brief Write a double word to the secure FLASH.
 * 
 */
int flash_secure_program_dw(uint32_t addr, uint64_t data64);

#endif // PLATFORM_FLASH_H