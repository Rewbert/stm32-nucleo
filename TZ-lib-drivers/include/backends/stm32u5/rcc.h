#ifndef BACKENDS_STM32U5_RCC_H
#define BACKENDS_STM32U5_RCC_H

#include "drivers/rcc.h"

/* No backend storage required — this driver accesses RCC registers directly via RCCx macro. */
void stm32u5_rcc_create(rcc_dev_t *dev);

/* STM32U5-specific PLL helper: configure PLL1 for a target SYSCLK frequency.
 * Typical values for 160 MHz: pll_m=1, pll_n=10, pll_div=1 (VCO=160 MHz, PLLR=1). */
void stm32u5_configure_pll(struct rcc_dev *dev,
                            struct flash_dev *flash,
                            uint32_t pll_n,
                            uint32_t pll_m,
                            uint32_t pll_div,
                            uint32_t target_sysclk_hz);

#endif // BACKENDS_STM32U5_RCC_H
