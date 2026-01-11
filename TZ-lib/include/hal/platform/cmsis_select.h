#ifndef CMSIS_SELECT_H
#define CMSIS_SELECT_H

#include "hal/platform/domain.h"

#if HAL_SECURE

// this must be renamed
#define GPIO(port)   GPIO##port_S
#define RCCx         RCC_S
#define EXTIx        EXTI_S
#define LPUART1x     LPUART1_S

#else

#define GPIO(port)   GPIO##port##_NS
#define RCCx         RCC_NS
#define EXTIx        EXTI_NS
#define LPUART1x     LPUART1_NS

#endif // HAL_SECURE

#endif // CMSIS_SELECT_H