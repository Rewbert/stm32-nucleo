#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/mpcbb.h"

#include "stm32l5xx.h"

typedef struct {
    GTZC_MPCBB_TypeDef *mpcbb;
} stm32l5_mpcbb_backend_t;

void stm32l5_set_superblocks(struct mpcbb_dev *dev, uint32_t first, uint32_t count, mpcbb_security_t sec) {
#if HAL_SECURE
    stm32l5_mpcbb_backend_t *backend = (stm32l5_mpcbb_backend_t*) dev->backend;

    uint32_t value;
    switch(sec) {
        case MPCBB_SECURE: value = 0xFFFFFFFF; break;
        case MPCBB_NONSECURE: value = 0x00000000; break;
        default: return;
    }

    for(int i = first; i < first + count; i++) {   
        backend->mpcbb->VCTR[i] = value;
    }
#endif
}

void stm32l5_lock(struct mpcbb_dev *dev) {
#if HAL_SECURE
    stm32l5_mpcbb_backend_t *backend = (stm32l5_mpcbb_backend_t*) dev->backend;
    // this write will disable any further configuration of this MPCBB's VTOR. Only a reset of the entire board will
    // reallow configuration. Call this only when you are done configuring.
    //
    // The effect of this is really that after the board has been configured at boot, it cannot be reconfigured
    // at runtime.
    backend->mpcbb->CR |= GTZC_MPCBB_CR_SRWILADIS_Msk;
#endif
}

static const mpcbb_driver_api_t stm32l5_mpcbb_api = {
    .set_superblocks = stm32l5_set_superblocks,
    .lock = stm32l5_lock,
};

void stm32l5_mpcbb_create(mpcbb_dev_t *dev,
                          GTZC_MPCBB_TypeDef *mpcbb,
                          stm32l5_mpcbb_backend_t *backend) {
#if HAL_SECURE
    backend->mpcbb = mpcbb;
    mpcbb->CR &= ~GTZC_MPCBB_CR_INVSECSTATE_Msk;

    dev->api = &stm32l5_mpcbb_api;
    dev->backend = backend;
#endif
}