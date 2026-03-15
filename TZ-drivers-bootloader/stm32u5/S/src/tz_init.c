#include "stm32u5xx.h"

#include "drivers/sau.h"
#include "drivers/mpcbb.h"

#include "board.h"

/**
 * @brief NOTES ON SRAM
 *
 * Rather than 256 bytes as for the stm32l5, the stm32u5 has blocks of 512 bytes.
 * Furthermore, a superblock is made up of 32 blocks, so each superblock is ~16KB.
 *
 * We configure, again, a 50/50 split of SRAM between the secure and nonsecure world. The SRAM sizes are
 *
 *  - SRAM1: 768 KB (48 superblocks)
 *  - SRAM2: 64 KB (4 superblocks)
 *  - SRAM3: 832 KB (52 superblocks)
 *  - SRAM5: 832 KB (52 superblocks)
 *
 * A total of 2496 KB SRAM (156 superblocks). These SRAM blocks are contiguous in memory, and we
 * can cleanly divide them. We want 78 superblocks per domain, and thus assign
 *
 * - SRAM1: all   48 secure
 * - SRAM2: all   4  secure
 * - SRAM3: first 26 secure, last 26 non-secure
 * - SRAM5: all   52 non-secure
 *
 * In address terms, SRAM starts at 0x30000000/0x20000000. Each half is 1248 KB, which is 0x138000 in HEX.
 * - Secure SRAM    spans from 0x30000000 to 0x30137FFF
 * - Nonsecure SRAM spans from 0x20138000 to 0x2026FFFF
 */

/**
 * @brief NOTES ON FLASH
 *
 * Flash is simpler, as there is just a single flash and no superblock nonsense. We have 4 MB of
 * flash, and again, we do a 50/50 split. There is no MPCBB to configure here, but we must
 * tell the SAU where these regions begin and end.
 *
 * The secure applications FLASH must be the first thing in FLASH, so the first half in our case
 * is designated secure, and the second half nonsecure.
 *
 * FLASH starts as 0x0C000000/0x08000000
 * Secure FLASH runs from 0x0C000000 to 0x0C1FFFFF, with the nonsecure callable region going into the
 * last 4 KB. The nonsecure callable region starts at 0x0C1FF000 and ends at 0x0C1FFFFF. Directly
 * after the nonsecure callable region, nonsecure FLASH starts. It runs from 0x08200000 to 0x083FFFFF.
 * 
 */

#define NUM_SAU_REGIONS 8

static const sau_region_t regions[NUM_SAU_REGIONS] = {
    SAU_REGION("Non-secure callable region",    0x0C1FF000, 0x0C1FFFFF, SAU_S_NSC),
    SAU_REGION("Non-secure FLASH",              0x08200000, 0x083FFFFF, SAU_NS),
    SAU_REGION("Non-secure SRAM",               0x20138000, 0x2026FFFF, SAU_NS),  /* 1248 KB: SRAM3[26-51] + SRAM5 */
    SAU_REGION("Non-secure peripherals",        0x40000000, 0x4FFFFFFF, SAU_NS),
    SAU_REGION("Non-secure external memories",  0x60000000, 0x9FFFFFFF, SAU_NS),
    SAU_REGION("Non-secure system memory",      0x0BF90000, 0x0BFA8FFF, SAU_NS),
    SAU_REGION("unused",                        0x00000000, 0x00000000, SAU_NS),
    SAU_REGION("unused",                        0x00000000, 0x00000000, SAU_NS),
};

static inline void mpcbb_configure(void) {
    mpcbb_dev_t *sram1 = board_mpcbb(0);
    mpcbb_dev_t *sram2 = board_mpcbb(1);
    mpcbb_dev_t *sram3 = board_mpcbb(2);
    mpcbb_dev_t *sram4 = board_mpcbb(3); /* GTZC2 — board_mpcbb enables RCC_GTZC2 lazily */
    mpcbb_dev_t *sram5 = board_mpcbb(4);

    mpcbb_set_superblocks(sram1, 0, 48, MPCBB_SECURE);
    mpcbb_set_superblocks(sram2, 0, 4, MPCBB_SECURE);
    mpcbb_set_superblocks(sram3,  0, 26, MPCBB_SECURE);
    mpcbb_set_superblocks(sram3, 26, 26, MPCBB_NONSECURE);
    mpcbb_set_superblocks(sram4, 0, 1, MPCBB_NONSECURE);
    mpcbb_set_superblocks(sram5, 0, 52, MPCBB_NONSECURE);

    mpcbb_lock(sram1);
    mpcbb_lock(sram2);
    mpcbb_lock(sram3);
    mpcbb_lock(sram4);
    mpcbb_lock(sram5);
}

#define NS_VECTOR_TABLE 0x08200000U

void nonsecure_reset_handler(void) {
    SCB_NS->VTOR = NS_VECTOR_TABLE;
    __asm volatile ("msr msp_ns, %0" :: "r" (*((uint32_t *) NS_VECTOR_TABLE)));

    uint32_t ns_reset_handler_addr = *((uint32_t *)(NS_VECTOR_TABLE + 4U));
    typedef void (*ns_reset_ptr_t)(void) __attribute__((cmse_nonsecure_call));
    ns_reset_ptr_t ns_reset_handler = (ns_reset_ptr_t) (void (*)(void))ns_reset_handler_addr;

    ns_reset_handler();
}

extern void main(void); /* secure main */

void tz_init(void) {
    sau_configure(regions, NUM_SAU_REGIONS);
    mpcbb_configure();

    main();

    nonsecure_reset_handler();
}
