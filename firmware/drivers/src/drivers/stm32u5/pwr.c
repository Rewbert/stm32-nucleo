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

/* Note, this only works if you configure it once from boot. Lowering the voltagge
 scaling does not work, as we do not clear the VOS bits before setting them. */
void stm32u5_set_voltage_scaling(struct pwr_dev *dev, pwr_voltage_scaling_t range) {
#if HAL_SECURE
    uint32_t val;
    switch(range) {
        case PWR_RANGE_1: val = 0x3U; break;
        case PWR_RANGE_2: val = 0x2U; break;
        case PWR_RANGE_3: val = 0x1U; break;
        case PWR_RANGE_4: val = 0x0U; break;
        default: return;
    }

    PWRx->VOSR |= (val << PWR_VOSR_VOS_Pos);

    while(!(PWRx->VOSR & PWR_VOSR_VOSRDY_Msk));

    if(range == PWR_RANGE_1 || range == PWR_RANGE_2) {
        PWRx->VOSR |= (0x1U << PWR_VOSR_BOOSTEN_Pos);
        while(!(PWRx->VOSR & PWR_VOSR_BOOSTRDY_Msk));
    }
#endif
}

static const pwr_driver_api_t stm32u5_pwr_api = {
    .enable_vddio2  = stm32u5_enable_vddio2,
    .disable_vddio2 = stm32u5_disable_vddio2,
    .set_voltage_scaling = stm32u5_set_voltage_scaling,
};

void stm32u5_pwr_create(pwr_dev_t *dev) {
    dev->api     = &stm32u5_pwr_api;
    dev->backend = NULL;
}
