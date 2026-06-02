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

static void stm32l5_set_voltage_scaling(struct pwr_dev *dev, pwr_voltage_scaling_t range) {
    (void)dev; (void)range; /* L5 runs at 110 MHz within its default voltage range */
}

static const pwr_driver_api_t stm32l5_pwr_api = {
    .enable_vddio2       = stm32l5_enable_vddio2,
    .disable_vddio2      = stm32l5_disable_vddio2,
    .set_voltage_scaling = stm32l5_set_voltage_scaling,
};

void stm32l5_pwr_create(pwr_dev_t *dev) {
    dev->api = &stm32l5_pwr_api;
    dev->backend = NULL;
}