#ifndef CMSIS_SELECT_H
#define CMSIS_SELECT_H

#include "domain/domain.h"

/* Pull in the MCU-specific CMSIS device header based on the build-time define.
 * This is the canonical way to get SysTick_Config, peripheral typedefs, etc. */
#if defined(STM32U5A5xx)
#include "stm32u5xx.h"
#elif defined(STM32L552xx)
#include "stm32l5xx.h"
#else
#error "Unknown MCU — define STM32U5A5xx or STM32L552xx"
#endif

#if HAL_SECURE

// this must be renamed
#define GPIO_CMSIS(port)   GPIO##port##_S
#define RCCx               RCC_S
#define PWRx               PWR_S
#define EXTIx              EXTI_S
#define LPUART1x           LPUART1_S
#define USART1x            USART1_S
#define FLASHx             FLASH_S
#define GTZC_TZSCx         GTZC_TZSC_S

#else

#define GPIO_CMSIS(port)   GPIO##port##_NS
#define RCCx               RCC_NS
#define PWRx               PWR_NS
#define EXTIx              EXTI_NS
#define LPUART1x           LPUART1_NS
#define USART1x            USART1_NS
#define FLASHx             FLASH_NS
#define GTZC_TZSCx         GTZC_TZSC_NS

#endif // HAL_SECURE

#endif // CMSIS_SELECT_H