
#include "persist.h"
#include "stm32l5xx.h"

/* To enable writing to the FLASH, we need to unlock writes, acording to the reference manual. Reading
is always OK, but writing is special. The reference manual says that it order to unlock writing we must
write two keys to the register mentioned below. The keys are written in plaintext in the reference manual,
and I assume they are the same on all these devices. I guess this sequence is required to prevent accidental
writing to flash.

If we write to this register but the sequence is wrong, the manual says that NSCR/SECCR registers
become locked until the next system reset.
FLASH is locked again by setting NSLOCK and SECLOCK bits in NSCR and SECCR register.*/
void flash_secure_unlock(void) {
    // if FLASH is locked
    if(FLASH_S->SECCR & FLASH_SECCR_SECLOCK) {
        // unlock it
        FLASH_S->SECKEYR = 0x45670123; // this is key 1, from the reference manual
        FLASH_S->SECKEYR = 0xCDEF89AB; // key 2
    }
}

void flash_secure_lock(void) {
    // if FLASH is unlocked
    if(!(FLASH_S->SECCR & FLASH_SECCR_SECLOCK)) {
        // lock it
        FLASH_S->SECCR |= (1 << FLASH_SECCR_SECLOCK_Pos);
    }
}

void flash_secure_clear_flags(void) {
    FLASH_S->SECSR = ( FLASH_SECSR_SECEOP
                     | FLASH_SECSR_SECOPERR
                     | FLASH_SECSR_SECPROGERR
                     | FLASH_SECSR_SECSIZERR
                     | FLASH_SECSR_SECPGAERR
                     | FLASH_SECSR_SECPGSERR
                     | FLASH_SECSR_SECWRPERR);
    while (FLASH_S->SECSR & FLASH_SECSR_SECBSY) { }
}

// erase a page
int flash_secure_erase_page(uint32_t bank, uint32_t page) {
    flash_secure_unlock();
    flash_secure_clear_flags();

    // Program SECCR: set SECPER, select bank (SECBKER), select page number (SECPNB)
    uint32_t seccr = FLASH_S->SECCR;
    seccr |= FLASH_SECCR_SECEOPIE;
    seccr |= FLASH_SECCR_SECPER; // this enables secure page erase
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

    // when FLASH is busy, this bit is set. This while loop ensures that we do nothing else
    // until flash is not busy anymore
    while (FLASH_S->SECSR & FLASH_SECSR_SECBSY) {}
    uint32_t st = FLASH_S->SECSR;
   
    // FLASH will indicate that it is done writing by setting the SECEOP bit. We can clear the bit by
    // again writing a zero into it.
    if (FLASH_S->SECSR & FLASH_SECSR_SECEOP) {
        FLASH_S->SECSR = FLASH_SECSR_SECEOP;
        FLASH_S->SECCR &= ~FLASH_SECCR_SECPER;
        return 1;
    }
    return 0;
}

// write (program) a double-word (64bit) into a specific address
int flash_secure_program_dw(uint32_t addr, uint64_t data64) {
    // addr must be in Secure Flash and 8-byte aligned
    flash_secure_unlock();
    flash_secure_clear_flags();

    // enable programming of secure flash
    FLASH_S->SECCR |= FLASH_SECCR_SECEOPIE | FLASH_SECCR_SECPG;

    *(volatile uint32_t *)addr       = (uint32_t)(data64 & 0xFFFFFFFFu);
    *(volatile uint32_t *)(addr + 4) = (uint32_t)(data64 >> 32);

    // wait until flash is not busy
    while (FLASH_S->SECSR & FLASH_SECSR_SECBSY) {}

    uint32_t st = FLASH_S->SECSR;
    int ok = (FLASH_S->SECSR & FLASH_SECSR_SECEOP) != 0;
    if (ok) FLASH_S->SECSR = FLASH_SECSR_SECEOP;

    const uint32_t ERR = FLASH_SECSR_SECWRPERR | FLASH_SECSR_SECPROGERR |
                         FLASH_SECSR_SECSIZERR | FLASH_SECSR_SECPGAERR  |
                         FLASH_SECSR_SECPGSERR | FLASH_SECSR_SECOPERR;

    // indicate that we are not programming anymore
    FLASH_S->SECCR &= ~FLASH_SECCR_SECPG;
    return ok;
}