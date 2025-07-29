

#include "stm32l5xx.h"

const uint32_t NS_VECTOR_TABLE = 0x08040000U;

#define NUM_SAU_REGIONS   8
#define RNR_MASK          0xFF
#define RBAR_MASK         0xFFFFFFE0
#define RLAR_MASK         0xFFFFFFE0
#define ATTRIBUTE_POS     1
#define ATTRIBUTE_MASK    (ATTRIBUTE_POS << 1)
#define SAU_REGION_ENABLE 0x1

/* Start addresses of the 8 SAU regions */
const uint32_t sau_start[8] = { 0x0C03F000, // last 4K of secure flash, marked as non-secure callable
                                0x08040000, // Start of non-secure flash
                                0x20018000, // start of non-secure SRAM1
                                0x40000000, // Non-secure mapped peripherals
                                0x60000000, // Non-secure external memories (not used)
                                0x0BF90000, // Non-secure system memory
                                0x00000000, // unused, non-secure
                                0x00000000, // unused, non-secure
                              };

/* End addresses of the 8 SAU regions */
const uint32_t sau_end[8] = { 0x0C04FFFF,
                              0x0807FFFF,
                              0x2002FFFF,
                              0x4FFFFFFF,
                              0x9FFFFFFF,
                              0x0BFA8FFF,
                              0x00000000,
                              0x00000000,
                            };

/* Security attribute of the 8 SAU regions; 1 = secure/non-secure callable, 0 = non-secure */
const uint32_t attribute[8] = { 1,
                                0,
                                0,
                                0,
                                0,
                                0,
                                0,
                                0,
                               };

/* Configure the Security Attribution Unit to designate parts of memory as non-secure.

NOTE: You must also configure the two options byes SECWM2_PSTRT and SECWM2_PEND. These designate part of the
second bank of flash as secure or non-secure. The default values of PSTRT=0x0 and PEND=0x7F, marking the whole
thing as secure.
To mark the whole thing as non-secure, we just need to make sure tht PSTRT > PEND. To do this, write
PSTRT=0x1 and PEND=0x0.
*/
void configure_sau(void) {
    for(int i = 0; i < NUM_SAU_REGIONS; i++) {
        SAU->RNR = i & RNR_MASK;
        SAU->RBAR = sau_start[i] & RBAR_MASK;
        SAU->RLAR = (sau_end[i] & RLAR_MASK)         |
                    ((attribute[i] << ATTRIBUTE_POS) & ATTRIBUTE_MASK) |
                    SAU_REGION_ENABLE;
    }

    SAU->CTRL = ((1 << SAU_CTRL_ENABLE_Pos) & SAU_CTRL_ENABLE_Msk);
}

/* Configure the Memory Protection unit infront of the SRAM1 to mark one half (first half) as secure and
the second half as non-secure. */
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

void initialise(void) {
    configure_sau();
    configure_mpcbb1();

    SCB_NS->VTOR = NS_VECTOR_TABLE;

    GTZC_MPCBB1->CR |= GTZC_MPCBB_CR_SRWILADIS_Msk;

    __asm volatile ("msr msp_ns, %0" :: "r" (*((uint32_t *) NS_VECTOR_TABLE)));

    uint32_t ns_reset_handler_addr = *((uint32_t *)(NS_VECTOR_TABLE + 4U));
    typedef void (*ns_reset_ptr_t)(void) __attribute__((cmse_nonsecure_call));
    ns_reset_ptr_t ns_reset_handler = (ns_reset_ptr_t) (void (*)(void))ns_reset_handler_addr;

    ns_reset_handler();
}

/***** SECURE USER APPLICATION *****/

void main(void) {
    initialise();
    while(1) {}
}
