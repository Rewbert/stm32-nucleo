#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/sau.h"

#include "stm32l5xx.h"

#define RNR_MASK          0xFF
#define RBAR_MASK         0xFFFFFFE0
#define RLAR_MASK         0xFFFFFFE0
#define ATTRIBUTE_POS     1
#define ATTRIBUTE_MASK    (ATTRIBUTE_POS << 1)
#define SAU_REGION_ENABLE 0x1

/**
 * @brief Don't prefix this by board name, as the SAU belongs to the CPU and not MCU. It should be the same across all
 * of these STM32 Nucleo boards.
 * 
 */
void sau_configure(const sau_region_t *regions, uint32_t count) {
#if HAL_SECURE
    if(count > 8) return;

    for(int i = 0; i < count; i++) {
        sau_region_t region = regions[i];

        SAU->RNR = i & RNR_MASK;
        SAU->RBAR = region.start & RBAR_MASK;
        SAU->RLAR = ( region.end & RLAR_MASK)                         |
                    ((region.attr << ATTRIBUTE_POS) & ATTRIBUTE_MASK) |
                    SAU_REGION_ENABLE;
    }

    SAU->CTRL = ((1 << SAU_CTRL_ENABLE_Pos) & SAU_CTRL_ENABLE_Msk);
#endif
}