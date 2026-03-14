#include <stddef.h>

#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/pwr.h"

#include "stm32u5xx.h"

/* On STM32U5, VDDIO2 validity is controlled via PWR_SVMCR.IO2SV (Supply Voltage
 * Monitoring Control Register), not via PWR_CR2.IOSV as on STM32L5. */

void stm32u5_enable_vddio2(struct pwr_dev *dev) {
#if HAL_SECURE
    PWRx->SVMCR |= PWR_SVMCR_IO2SV;
#endif
}

void stm32u5_disable_vddio2(struct pwr_dev *dev) {
#if HAL_SECURE
    PWRx->SVMCR &= ~PWR_SVMCR_IO2SV;
#endif
}

static const pwr_driver_api_t stm32u5_pwr_api = {
    .enable_vddio2  = stm32u5_enable_vddio2,
    .disable_vddio2 = stm32u5_disable_vddio2,
};

void stm32u5_pwr_create(pwr_dev_t *dev) {
    dev->api     = &stm32u5_pwr_api;
    dev->backend = NULL;
}
