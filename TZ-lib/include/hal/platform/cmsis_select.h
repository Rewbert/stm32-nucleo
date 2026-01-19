#ifndef CMSIS_SELECT_H
#define CMSIS_SELECT_H

#include "hal/platform/domain.h"

#if HAL_SECURE

// this must be renamed
#define GPIO_CMSIS(port)   GPIO##port##_S
#define RCCx               RCC_S
#define PWRx               PWR_S
#define EXTIx              EXTI_S
#define LPUART1x           LPUART1_S
#define FLASHx             FLASH_S
#define GTZC_TZSCx         GTZC_TZSC_S

#else

#define GPIO_CMSIS(port)   GPIO##port##_NS
#define RCCx               RCC_NS
#define PWRx               PWR_NS
#define EXTIx              EXTI_NS
#define LPUART1x           LPUART1_NS
#define FLASHx             FLASH_NS
#define GTZC_TZSCx         GTZC_TZSC_NS

#endif // HAL_SECURE

#endif // CMSIS_SELECT_H