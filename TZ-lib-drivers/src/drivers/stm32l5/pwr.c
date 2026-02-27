#include <stddef.h>

#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/pwr.h"

#include "stm32l5xx.h"

void stm32l5_enable_vddio2(struct pwr_dev *dev) {
#if HAL_SECURE
    PWRx->CR2 |= PWR_CR2_IOSV;
#endif
}
void stm32l5_disable_vddio2(struct pwr_dev *dev) {
#if HAL_SECURE
    PWRx->CR2 &= ~PWR_CR2_IOSV;
#endif
}

static const pwr_driver_api_t stm32l5_pwr_api = {
    .enable_vddio2 = stm32l5_enable_vddio2,
    .disable_vddio2 = stm32l5_disable_vddio2,
};

void stm32l5_pwr_create(pwr_dev_t *dev) {
    dev->api = &stm32l5_pwr_api;
    dev->backend = NULL;
}