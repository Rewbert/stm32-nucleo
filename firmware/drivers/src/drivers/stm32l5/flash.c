
#include <string.h>

#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32l5/flash.h"

#include "stm32l5xx.h"

/* ---- Flash geometry (STM32L5) ---------------------------------------------
 * 2 KB erase pages, 128 pages per 256 KB bank, 64-bit (double-word) program
 * unit. FL_BASE is this domain's flash alias: secure builds drive the SECxx
 * registers against the 0x0C… alias, non-secure builds the NSxx registers
 * against the 0x08… alias. */
#define FL_PAGE_SIZE      0x800u
#define FL_PAGES_PER_BANK 128u
#define FL_PROG_WORDS     2u
#define FL_PROG_SIZE      (FL_PROG_WORDS * 4u)

#if HAL_SECURE
#  define FL_BASE  FLASH_BASE_S
#else
#  define FL_BASE  FLASH_BASE_NS
#endif

/* Unlock keys (identical across STM32 families; not in the CMSIS device header). */
#define FL_KEY1 0x45670123u
#define FL_KEY2 0xCDEF89ABu

/* Domain-selected control/status/key registers and the bits we use. The bit
 * positions are identical on the secure and non-secure banks; only the CMSIS
 * macro names differ (SEC… vs NS…). */
#if HAL_SECURE
#  define FL_CR(f)    ((f)->SECCR)
#  define FL_SR(f)    ((f)->SECSR)
#  define FL_KEYR(f)  ((f)->SECKEYR)
#  define FL_PG        FLASH_SECCR_SECPG
#  define FL_PER       FLASH_SECCR_SECPER
#  define FL_PNB_Pos   FLASH_SECCR_SECPNB_Pos
#  define FL_BKER      FLASH_SECCR_SECBKER
#  define FL_STRT      FLASH_SECCR_SECSTRT
#  define FL_LOCK      FLASH_SECCR_SECLOCK
#  define FL_BSY       FLASH_SECSR_SECBSY
#  define FL_EOP       FLASH_SECSR_SECEOP
#else
#  define FL_CR(f)    ((f)->NSCR)
#  define FL_SR(f)    ((f)->NSSR)
#  define FL_KEYR(f)  ((f)->NSKEYR)
#  define FL_PG        FLASH_NSCR_NSPG
#  define FL_PER       FLASH_NSCR_NSPER
#  define FL_PNB_Pos   FLASH_NSCR_NSPNB_Pos
#  define FL_BKER      FLASH_NSCR_NSBKER
#  define FL_STRT      FLASH_NSCR_NSSTRT
#  define FL_LOCK      FLASH_NSCR_NSLOCK
#  define FL_BSY       FLASH_NSSR_NSBSY
#  define FL_EOP       FLASH_NSSR_NSEOP
#endif

/* Error flags (write-1-to-clear) live in SR bits 1..15; clearing EOP too is
 * harmless. BSY (bit 16) is read-only, so a 16-bit clear leaves it untouched. */
#define FL_SR_CLEAR  0x0000FFFFu

static void flash_unlock(FLASH_TypeDef *flash) {
    if (FL_CR(flash) & FL_LOCK) {
        FL_KEYR(flash) = FL_KEY1;
        FL_KEYR(flash) = FL_KEY2;
    }
}

static void flash_wait_idle(FLASH_TypeDef *flash) {
    while (FL_SR(flash) & FL_BSY) { }
}

void stm32l5_set_latency(struct flash_dev *dev, uint32_t wait_states) {
#if HAL_SECURE
    if(wait_states > 16 || wait_states < 0) return;

    stm32l5_flash_backend_t *backend = (stm32l5_flash_backend_t*) dev->backend;
    backend->flash->ACR = (backend->flash->ACR & ~FLASH_ACR_LATENCY) | (wait_states << FLASH_ACR_LATENCY_Pos); // 4 is right for me

    while ((backend->flash->ACR & FLASH_ACR_LATENCY) != (wait_states << FLASH_ACR_LATENCY_Pos));
#endif
}

uint32_t stm32l5_get_latency(struct flash_dev *dev) {
#if HAL_SECURE
    stm32l5_flash_backend_t *backend = (stm32l5_flash_backend_t*) dev->backend;
    return (backend->flash->ACR & FLASH_ACR_LATENCY) >> FLASH_ACR_LATENCY_Pos;
#endif
    return 0;
}

/* read: copy straight from memory-mapped flash (no controller needed). */
static int stm32l5_flash_read(struct flash_dev *dev, uint32_t addr, void *buf, uint32_t len) {
    (void)dev;
    memcpy(buf, (const void *)(uintptr_t)addr, len);
    return 0;
}

/* program: write whole program-units (double-word). addr and len must be
 * program-unit aligned. */
static int stm32l5_flash_program(struct flash_dev *dev, uint32_t addr, const void *data, uint32_t len) {
    FLASH_TypeDef *flash = ((stm32l5_flash_backend_t *)dev->backend)->flash;

    if ((addr % FL_PROG_SIZE) != 0u || (len % FL_PROG_SIZE) != 0u) {
        return -1;
    }

    const uint32_t *src = (const uint32_t *)data;
    volatile uint32_t *dst = (volatile uint32_t *)(uintptr_t)addr;

    flash_unlock(flash);

    for (uint32_t u = 0; u < len / FL_PROG_SIZE; u++) {
        flash_wait_idle(flash);
        FL_SR(flash) = FL_SR_CLEAR;

        FL_CR(flash) |= FL_PG;
        for (uint32_t w = 0; w < FL_PROG_WORDS; w++) {
            *dst++ = *src++;
        }
        flash_wait_idle(flash);

        if (FL_SR(flash) & FL_EOP) {
            FL_SR(flash) = FL_EOP;
        }
        FL_CR(flash) &= ~FL_PG;
    }

    return 0;
}

/* erase_page: page-erase the erase page containing addr. */
static int stm32l5_flash_erase_page(struct flash_dev *dev, uint32_t addr) {
    FLASH_TypeDef *flash = ((stm32l5_flash_backend_t *)dev->backend)->flash;

    uint32_t page = (addr - FL_BASE) / FL_PAGE_SIZE;
    uint32_t bker = page / FL_PAGES_PER_BANK;
    uint32_t pnb  = page % FL_PAGES_PER_BANK;

    flash_unlock(flash);
    flash_wait_idle(flash);
    FL_SR(flash) = FL_SR_CLEAR;

    /* Write CR fresh so a stale PNB cannot leak in; LOCK is 0 after unlock. */
    uint32_t cr = FL_PER | (pnb << FL_PNB_Pos);
    if (bker) {
        cr |= FL_BKER;
    }
    FL_CR(flash) = cr;
    FL_CR(flash) = cr | FL_STRT;

    flash_wait_idle(flash);
    FL_CR(flash) &= ~FL_PER;

    return 0;
}

static uint32_t stm32l5_flash_prog_size(struct flash_dev *dev) {
    (void)dev;
    return FL_PROG_SIZE;
}

static uint32_t stm32l5_flash_page_size(struct flash_dev *dev) {
    (void)dev;
    return FL_PAGE_SIZE;
}

static const flash_driver_api_t stm32l5_flash_api = {
    .set_latency = stm32l5_set_latency,
    .get_latency = stm32l5_get_latency,
    .read        = stm32l5_flash_read,
    .program     = stm32l5_flash_program,
    .erase_page  = stm32l5_flash_erase_page,
    .prog_size   = stm32l5_flash_prog_size,
    .page_size   = stm32l5_flash_page_size,
};

void stm32l5_flash_create(flash_dev_t *dev,
                          FLASH_TypeDef *flash,
                          stm32l5_flash_backend_t *backend) {
    backend->flash = flash;

    dev->api = &stm32l5_flash_api;
    dev->backend = backend;
}
