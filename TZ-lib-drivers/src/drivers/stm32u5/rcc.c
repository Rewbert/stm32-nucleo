#include <stddef.h>

#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/flash.h"
#include "drivers/rcc.h"

#include "stm32u5xx.h"

/**
 * @brief Maps each rcc_periph_t to the register and bit required to enable/disable its clock.
 *
 * Key differences vs STM32L5:
 *  - GPIO clocks are in AHB2ENR1 (not AHB2ENR)
 *  - LPUART1 clock is in APB3ENR (moved from APB1 to APB3 bus)
 *  - PWR clock is in AHB3ENR (moved from APB1 to AHB3)
 *  - GTZC1 enable is RCC_AHB1ENR_GTZC1EN (renamed from GTZCEN)
 *  - GTZC2 enable is RCC_AHB3ENR_GTZC2EN (new on U5)
 */
typedef struct {
    volatile uint32_t *reg;
    uint32_t bit;
} rcc_desc_t;

static const rcc_desc_t rcc_table[RCC_PERIPH_COUNT] = {
    [RCC_GPIOA] = { &RCCx->AHB2ENR1, RCC_AHB2ENR1_GPIOAEN },
    [RCC_GPIOB] = { &RCCx->AHB2ENR1, RCC_AHB2ENR1_GPIOBEN },
    [RCC_GPIOC] = { &RCCx->AHB2ENR1, RCC_AHB2ENR1_GPIOCEN },
    [RCC_GPIOD] = { &RCCx->AHB2ENR1, RCC_AHB2ENR1_GPIODEN },
    [RCC_GPIOE] = { &RCCx->AHB2ENR1, RCC_AHB2ENR1_GPIOEEN },
    [RCC_GPIOF] = { &RCCx->AHB2ENR1, RCC_AHB2ENR1_GPIOFEN },
    [RCC_GPIOG] = { &RCCx->AHB2ENR1, RCC_AHB2ENR1_GPIOGEN },
    [RCC_GPIOH] = { &RCCx->AHB2ENR1, RCC_AHB2ENR1_GPIOHEN },

    [RCC_LPUART1] = { &RCCx->APB3ENR, RCC_APB3ENR_LPUART1EN },
    [RCC_PWR]     = { &RCCx->AHB3ENR, RCC_AHB3ENR_PWREN     },
    [RCC_GTZC]    = { &RCCx->AHB1ENR, RCC_AHB1ENR_GTZC1EN   },
    [RCC_GTZC2]   = { &RCCx->AHB3ENR, RCC_AHB3ENR_GTZC2EN   },
};

static void stm32u5_rcc_enable(struct rcc_dev *dev, rcc_periph_t periph) {
#if HAL_SECURE
    (void)dev;

    if (periph >= RCC_PERIPH_COUNT) return;

    const rcc_desc_t *d = &rcc_table[periph];
    if (!d->reg) return;
    *d->reg |= d->bit;
    (void)*d->reg;
#endif
}

static void stm32u5_rcc_disable(struct rcc_dev *dev, rcc_periph_t periph) {
#if HAL_SECURE
    (void)dev;

    if (periph >= RCC_PERIPH_COUNT) return;

    const rcc_desc_t *d = &rcc_table[periph];
    if (!d->reg) return;
    *d->reg &= ~d->bit;
    (void)*d->reg;
#endif
}

static int stm32u5_rcc_is_enabled(struct rcc_dev *dev, rcc_periph_t periph) {
#if HAL_SECURE
    (void)dev;

    if (periph >= RCC_PERIPH_COUNT) return 0;

    const rcc_desc_t *d = &rcc_table[periph];
    if (!d->reg) return 0;
    return (*d->reg & d->bit) != 0;
#endif
    return 0;
}

typedef struct {
    volatile uint32_t *reg;
    uint32_t mask;
    uint32_t pos;
    const uint8_t clock_map[RCC_PERIPHERAL_CLOCK_COUNT];
} rcc_clock_mux_desc_t;

/**
 * LPUART1 clock mux moved from CCIPR1 (L5) to CCIPR3 (U5) and is now 3 bits wide.
 * Encoding: 000=PCLK3, 001=SYSCLK, 010=HSI16, 011=LSE, 100=MSIK
 */
static const rcc_clock_mux_desc_t clock_mux_table[] = {
    [RCC_LPUART1] = {
        .reg       = &RCCx->CCIPR3,
        .mask      = RCC_CCIPR3_LPUART1SEL_Msk,
        .pos       = RCC_CCIPR3_LPUART1SEL_Pos,
        .clock_map = {
            [RCC_PCKL1]  = 0x0U,  /* PCLK3 on U5, exposed as PCLK1 in the generic enum */
            [RCC_SYSCLK] = 0x1U,
            [RCC_HSI16]  = 0x2U,
            [RCC_LSE]    = 0x3U,
            [RCC_MSI]    = 0x4U,
        },
    },
};

void stm32u5_rcc_set_peripheral_clock(struct rcc_dev *dev, rcc_periph_t periph, rcc_periph_clock_source_t clock) {
#if HAL_SECURE
    if(periph >= RCC_PERIPH_COUNT) return;

    const rcc_clock_mux_desc_t *desc = &clock_mux_table[periph];

    if(!desc->reg)                          return;
    if(clock >= RCC_PERIPHERAL_CLOCK_COUNT) return;

    uint32_t val = desc->clock_map[clock];
    *desc->reg &= ~(desc->mask);
    *desc->reg |= (val << desc->pos);
    (void)*desc->reg;
#endif
}

#if HAL_SECURE
/**
 * @brief Set FLASH wait states based on the target SYSCLK.
 *
 * STM32U5 at VOS1 (high-performance) voltage range requires:
 *   ≤32 MHz  → 0 WS
 *   ≤64 MHz  → 1 WS
 *   ≤96 MHz  → 2 WS
 *   ≤128 MHz → 3 WS
 *   ≤160 MHz → 4 WS
 */
static inline void stm32u5_rcc_set_wait_states(struct flash_dev *flash, uint32_t target_sysclk_hz) {
    uint32_t wait_states;

    if      (target_sysclk_hz <=  32000000) wait_states = 0;
    else if (target_sysclk_hz <=  64000000) wait_states = 1;
    else if (target_sysclk_hz <=  96000000) wait_states = 2;
    else if (target_sysclk_hz <= 128000000) wait_states = 3;
    else                                     wait_states = 4; /* cap at 160 MHz */

    flash_set_latency(flash, wait_states);
    while(flash_get_latency(flash) != wait_states);
}
#endif

/**
 * @brief Configure PLL1 and switch SYSCLK to it.
 *
 * U5 PLL1 is sourced from MSIS (Multi-Speed Internal oscillator, ~4 MHz at reset).
 * Typical parameters for 160 MHz: pll_m=1, pll_n=40, pll_div=1 (VCO=160 MHz, PLLR/1).
 * Note: the U5 PLLCFGR layout differs from L5 — PLLM is zero-based here.
 */
void stm32u5_configure_pll(struct rcc_dev *dev,
                            struct flash_dev *flash,
                            uint32_t pll_n,
                            uint32_t pll_m,
                            uint32_t pll_div,
                            uint32_t target_sysclk_hz) {
#if HAL_SECURE
    if (RCCx->CR & RCC_CR_PLL1ON) {
        RCCx->CR &= ~RCC_CR_PLL1ON;
        while (RCCx->CR & RCC_CR_PLL1RDY);
    }

    stm32u5_rcc_set_wait_states(flash, target_sysclk_hz);

    /* Ensure MSIS is running at ~4 MHz (reset default, range 4) */
    RCCx->CR |= RCC_CR_MSISON;
    while (!(RCCx->CR & RCC_CR_MSISRDY));

    RCCx->PLL1CFGR = 0;
    RCCx->PLL1CFGR |= RCC_PLL1CFGR_PLL1SRC_0; /* Select MSIS as PLL1 source */
    RCCx->PLL1CFGR |= ((pll_m - 1) << RCC_PLL1CFGR_PLL1M_Pos);
    /* PLL1N lives in PLL1DIVR, not PLL1CFGR */
    RCCx->PLL1DIVR = (  ((pll_div - 1) << RCC_PLL1DIVR_PLL1R_Pos)
                      | ((pll_n   - 1) << RCC_PLL1DIVR_PLL1N_Pos)
                      );
    RCCx->PLL1CFGR |= RCC_PLL1CFGR_PLL1REN; /* Enable PLLR output */

    RCCx->CR |= RCC_CR_PLL1ON;
    while (!(RCCx->CR & RCC_CR_PLL1RDY));

    /* Switch SYSCLK to PLL1R */
    RCCx->CFGR1 = (RCCx->CFGR1 & ~RCC_CFGR1_SW) | (0x3U << RCC_CFGR1_SW_Pos);
    while ((RCCx->CFGR1 & RCC_CFGR1_SWS) != (0x3U << RCC_CFGR1_SWS_Pos));
#endif
}

static const rcc_driver_api_t stm32u5_rcc_api = {
    .enable               = stm32u5_rcc_enable,
    .disable              = stm32u5_rcc_disable,
    .is_enabled           = stm32u5_rcc_is_enabled,
    .set_peripheral_clock = stm32u5_rcc_set_peripheral_clock,
    .configure_pll        = stm32u5_configure_pll,
};

void stm32u5_rcc_create(rcc_dev_t *dev) {
    dev->api     = &stm32u5_rcc_api;
    dev->backend = NULL;
}
