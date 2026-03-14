#include "stm32u5xx.h"

#include "drivers/sau.h"
#include "drivers/mpcbb.h"

#include "board.h"

/* U5 memory split:
 *   Secure flash  : 0x0C000000 – 0x0C1FFFFF (2 MB, first half)
 *   NS flash      : 0x08200000 – 0x083FFFFF (2 MB, second half, NS-bus alias)
 *   Secure SRAM1  : 0x30000000 – 0x3005FFFF (384 KB, superblocks 0-23)
 *   NS SRAM1      : 0x20060000 – 0x200BFFFF (384 KB, superblocks 24-47, NS-bus alias)
 */

#define NUM_SAU_REGIONS 8

static const sau_region_t regions[NUM_SAU_REGIONS] = {
    SAU_REGION("Non-secure callable region",    0x0C1FF000, 0x0C1FFFFF, SAU_S_NSC),
    SAU_REGION("Non-secure FLASH",              0x08200000, 0x083FFFFF, SAU_NS),
    SAU_REGION("Non-secure SRAM1",              0x20060000, 0x200BFFFF, SAU_NS),
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

    /* SRAM1: 48 superblocks total; first 24 (384 KB) secure, rest NS */
    mpcbb_set_superblocks(sram1,  0, 24, MPCBB_SECURE);
    mpcbb_set_superblocks(sram1, 24, 24, MPCBB_NONSECURE);

    /* SRAM2-5: all non-secure */
    mpcbb_set_superblocks(sram2, 0,  4, MPCBB_NONSECURE);
    mpcbb_set_superblocks(sram3, 0, 32, MPCBB_NONSECURE);
    mpcbb_set_superblocks(sram4, 0,  1, MPCBB_NONSECURE);
    mpcbb_set_superblocks(sram5, 0, 48, MPCBB_NONSECURE);

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
