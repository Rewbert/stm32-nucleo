#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32u5/mpcbb.h"
#include "drivers/mpcbb.h"

#include "stm32u5xx.h"

/* Key difference vs STM32L5: the MPCBB vector registers are named SECCFGR[] on U5
 * (vs VCTR[] on L5). Each register controls 32 blocks of 512 B = 16 KB (one superblock).
 * STM32L5 superblock = 8 KB (256-byte blocks), STM32U5 superblock = 16 KB (512-byte blocks). */

void stm32u5_set_superblocks(struct mpcbb_dev *dev, uint32_t first, uint32_t count, mpcbb_security_t sec) {
#if HAL_SECURE
    stm32u5_mpcbb_backend_t *backend = (stm32u5_mpcbb_backend_t*) dev->backend;

    uint32_t value;
    switch(sec) {
        case MPCBB_SECURE:    value = 0xFFFFFFFF; break;
        case MPCBB_NONSECURE: value = 0x00000000; break;
        default: return;
    }

    for(uint32_t i = first; i < first + count; i++) {
        backend->mpcbb->SECCFGR[i] = value;
    }
#endif
}

void stm32u5_lock(struct mpcbb_dev *dev) {
#if HAL_SECURE
    stm32u5_mpcbb_backend_t *backend = (stm32u5_mpcbb_backend_t*) dev->backend;
    /* Freeze further SECCFGR writes until next reset. */
    backend->mpcbb->CR |= GTZC_MPCBB_CR_SRWILADIS_Msk;
#endif
}

static const mpcbb_driver_api_t stm32u5_mpcbb_api = {
    .set_superblocks = stm32u5_set_superblocks,
    .lock            = stm32u5_lock,
};

void stm32u5_mpcbb_create(mpcbb_dev_t *dev,
                           GTZC_MPCBB_TypeDef *mpcbb,
                           stm32u5_mpcbb_backend_t *backend) {
#if HAL_SECURE
    backend->mpcbb = mpcbb;
    mpcbb->CR &= ~GTZC_MPCBB_CR_INVSECSTATE_Msk;

    dev->api     = &stm32u5_mpcbb_api;
    dev->backend = backend;
#endif
}
