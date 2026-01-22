
#include "hal/platform/domain.h"
#include "hal/platform/cmsis_select.h"
#include "hal/platform/flash.h"

/**
 * @brief Erase a page of the FLASH memory, identified by the memory bank and page. This sets
 * all the bits in the page to 1.
 * 
 */
int flash_secure_erase_page(uint32_t bank, uint32_t page) {
#if !HAL_SECURE
    return 0;
#else
    platform_flash_secure_unlock();
    platform_flash_clear_secure_flags();

    // Program SECCR: set SECPER, select bank (SECBKER), select page number (SECPNB)
    uint32_t seccr = FLASHx->SECCR;
    seccr |= (FLASH_SECCR_SECEOPIE | FLASH_SECCR_SECPER); // enables secure page erase

    if (bank == 2) {
        // if SECBKER is 0, bank 1 is selected, and if it is 1, bank 2 is selected.
        // the default value after reset is 0, meaning we've selected bank 1
        // seccr |= FLASH_SECCR_SECBKER writes a 1 to it, selecting bank 2
        seccr |= FLASH_SECCR_SECBKER;
    } else {
        // this line ensures that we put a 0 in SECBKER, choosing bank 1
        seccr &= ~FLASH_SECCR_SECBKER;
    }
    // these two lines decide which page we are resetting in the selected bank
    seccr &= ~FLASH_SECCR_SECPNB_Msk;
    seccr |= (page << FLASH_SECCR_SECPNB_Pos) & FLASH_SECCR_SECPNB_Msk;
    FLASH_S->SECCR = seccr;

    // start actually erasing the flash
    FLASH_S->SECCR |= FLASH_SECCR_SECSTRT;

    platform_flash_wait_busy();
   
    // FLASH will indicate that it is done writing by setting the SECEOP bit. We can clear the bit by
    // again writing a zero into it.
    int ok = (FLASHx->SECSR & FLASH_SECSR_SECEOP) != 0;
    if (ok) {
        FLASHx->SECSR = FLASH_SECSR_SECEOP;
    }
    FLASHx->SECCR &= ~FLASH_SECCR_SECPER;

    platform_flash_secure_lock();
    return ok;
#endif
}

/**
 * @brief Write a double word to the secure FLASH.
 * 
 */
int flash_secure_program_dw(uint32_t addr, uint64_t data64) {
#if HAL_SECURE
    // addr must be in Secure Flash and 8-byte aligned
    platform_flash_secure_unlock();
    platform_flash_clear_secure_flags();

    // enable programming of secure flash
    FLASHx->SECCR |= FLASH_SECCR_SECEOPIE | FLASH_SECCR_SECPG;

    *(volatile uint32_t *)addr       = (uint32_t)(data64 & 0xFFFFFFFFu);
    *(volatile uint32_t *)(addr + 4) = (uint32_t)(data64 >> 32);

    platform_flash_wait_busy();

    uint32_t st = FLASHx->SECSR;
    int ok = (FLASHx->SECSR & FLASH_SECSR_SECEOP) != 0;
    if (ok) FLASHx->SECSR = FLASH_SECSR_SECEOP;

    const uint32_t ERR = FLASH_SECSR_SECWRPERR | FLASH_SECSR_SECPROGERR |
                         FLASH_SECSR_SECSIZERR | FLASH_SECSR_SECPGAERR  |
                         FLASH_SECSR_SECPGSERR | FLASH_SECSR_SECOPERR;

    // indicate that we are not programming anymore
    FLASHx->SECCR &= ~FLASH_SECCR_SECPG;

    platform_flash_secure_lock();
    return ok;
#endif
}