#include "stm32l5xx.h"

#include "drivers/sau.h"
#include "drivers/mpcbb.h"

#include "board.h"

#define NUM_SAU_REGIONS 8

static const sau_region_t regions[NUM_SAU_REGIONS] = {
    SAU_REGION("Non-secure callable region",              0x0C03F000, 0x0C04FFFF, SAU_S_NSC),
    SAU_REGION("Start of non-secure FLASH",               0x08040000, 0x0807FFFF, SAU_NS),
    SAU_REGION("Start of non-secure SRAM",                0x20020000, 0x2003FFFF, SAU_NS),
    SAU_REGION("Non-secure mapped peripherals",           0x40000000, 0x4FFFFFFF, SAU_NS),
    SAU_REGION("Non-secure external memories (not used)", 0x60000000, 0x9FFFFFFF, SAU_NS),
    SAU_REGION("Non-secure system memory",                0x0BF90000, 0x0BFA8FFF, SAU_NS),
    SAU_REGION("unused - non-secure",                     0x00000000, 0x00000000, SAU_NS),
    SAU_REGION("unused - non-secure",                     0x00000000, 0x00000000, SAU_NS),
};

static inline void mpcbb_configure() {
    mpcbb_dev_t *sram1 = board_mpcbb(0);
    mpcbb_dev_t *sram2 = board_mpcbb(1);

    mpcbb_set_superblocks(sram1, 0,  16, MPCBB_SECURE);
    mpcbb_set_superblocks(sram1, 16, 8,  MPCBB_NONSECURE);
    mpcbb_set_superblocks(sram2, 0,  8,  MPCBB_NONSECURE);

    mpcbb_lock(sram1);
    mpcbb_lock(sram2);
}

#define NS_VECTOR_TABLE 0x08040000U

void nonsecure_reset_handler() {
    SCB_NS->VTOR = NS_VECTOR_TABLE;
    __asm volatile ("msr msp_ns, %0" :: "r" (*((uint32_t *) NS_VECTOR_TABLE)));

    uint32_t ns_reset_handler_addr = *((uint32_t *)(NS_VECTOR_TABLE + 4U));
    typedef void (*ns_reset_ptr_t)(void) __attribute__((cmse_nonsecure_call));
    ns_reset_ptr_t ns_reset_handler = (ns_reset_ptr_t) (void (*)(void))ns_reset_handler_addr;

    ns_reset_handler();
}

extern void main(void); // secure main

void tz_init(void) {
    sau_configure(regions, NUM_SAU_REGIONS);
    mpcbb_configure();

    main();

    nonsecure_reset_handler();
}