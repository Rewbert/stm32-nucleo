
#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32l5/flash.h"

#include "stm32l5xx.h"

void stm32l5_set_latency(struct flash_dev *dev, uint32_t wait_states) {
#if HAL_SECURE
    if(wait_states > 16 || wait_states < 0) return;

    stm32l5_flash_backend_t *backend = (stm32l5_flash_backend_t*) dev->backend;
    backend->flash->ACR = (backend->flash->ACR & ~FLASH_ACR_LATENCY) | (wait_states << FLASH_ACR_LATENCY_Pos); // 4 is right for me

    while ((backend->flash->ACR & FLASH_ACR_LATENCY) != (wait_states << FLASH_ACR_LATENCY_Pos));
#endif
}

uint32_t stm32l5_get_latency(struct flash_dev *dev) {
#if HAL_SECURE
    stm32l5_flash_backend_t *backend = (stm32l5_flash_backend_t*) dev->backend;
    return (backend->flash->ACR & FLASH_ACR_LATENCY) >> FLASH_ACR_LATENCY_Pos;
#endif
    return 0;
}

static const flash_driver_api_t stm32l5_flash_api = {
    .set_latency = stm32l5_set_latency,
    .get_latency = stm32l5_get_latency,
};

void stm32l5_flash_create(flash_dev_t *dev,
                          FLASH_TypeDef *flash,
                          stm32l5_flash_backend_t *backend) {
    backend->flash = flash;

    dev->api = &stm32l5_flash_api;
    dev->backend = backend;
}