

#include "stm32l5xx.h"

// #define NS_VECTOR_TABLE 0x08040000

const uint32_t NS_VECTOR_TABLE = 0x08040000U;

void configure_mpcbb1(void);

void main(void) {
    configure_mpcbb1();

    SCB_NS->VTOR = NS_VECTOR_TABLE;

    GTZC_MPCBB1->CR |= GTZC_MPCBB_CR_SRWILADIS_Msk;

    __asm volatile ("msr msp_ns, %0" :: "r" (*((uint32_t *) NS_VECTOR_TABLE)));

    uint32_t ns_reset_handler_addr = *((uint32_t *)(NS_VECTOR_TABLE + 4U));
    typedef void (*ns_reset_ptr_t)(void) __attribute__((cmse_nonsecure_call));
    ns_reset_ptr_t ns_reset_handler = (ns_reset_ptr_t) (void (*)(void))ns_reset_handler_addr;

    ns_reset_handler();

    while(1) {}
}

void configure_mpcbb1(void) {
    volatile int dummy;

    // Enable global trustzone controller
    RCC->AHB1ENR |= RCC_AHB1ENR_GTZCEN;
    dummy = RCC->AHB1ENR;
    dummy = RCC->AHB1ENR;

    /* SRAM1 is 192KB large, and is divided into 256 byte blocks, meaning there are 768 blocks.
    They are in turn organised into super-blocks of 32 blocks each, meaning there are 24 such super blocks.
    Each such super-block can be configured through a register to define the security aspect. A 1 indicates that
    the block is secure and a 0 that it is nonsecure, so 0xFFFFFFFF means that the whole super-block is
    secure.

    Since we assign the lower half of SRAM1 to the secure world, we set the first 12 such registers all to
    0xFFFFFFFF, and the next 12 registers to 0x00000000.

    */
    GTZC_MPCBB1->CR &= ~GTZC_MPCBB_CR_INVSECSTATE_Msk;
    GTZC_MPCBB1->VCTR[0]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[1]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[2]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[3]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[4]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[5]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[6]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[7]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[8]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[9]  = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[10] = 0xFFFFFFFF;
    GTZC_MPCBB1->VCTR[11] = 0xFFFFFFFF;

    GTZC_MPCBB1->VCTR[12] = 0x00000000;
    GTZC_MPCBB1->VCTR[13] = 0x00000000;
    GTZC_MPCBB1->VCTR[14] = 0x00000000;
    GTZC_MPCBB1->VCTR[15] = 0x00000000;
    GTZC_MPCBB1->VCTR[16] = 0x00000000;
    GTZC_MPCBB1->VCTR[17] = 0x00000000;
    GTZC_MPCBB1->VCTR[18] = 0x00000000;
    GTZC_MPCBB1->VCTR[19] = 0x00000000;
    GTZC_MPCBB1->VCTR[20] = 0x00000000;
    GTZC_MPCBB1->VCTR[21] = 0x00000000;
    GTZC_MPCBB1->VCTR[22] = 0x00000000;
    GTZC_MPCBB1->VCTR[23] = 0x00000000;

}

// 4003 2400 - 4003 33ff