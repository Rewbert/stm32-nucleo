#include <stddef.h>

#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/flash.h"
#include "drivers/rcc.h"

#include "stm32l5xx.h"

/**
 * @brief This struct describes what is required to enable the power to a peripheral. You need the register in
 * which the control bit is, and the bit itself.
 */
typedef struct {
    volatile uint32_t *reg;
    uint32_t bit;
} rcc_desc_t;

/**
 * @brief This table records everything required to enable power to peripherals. With this table, I do not need
 * to have giant switch cases, matching the peripheral.
 *
 * This syntax was new to me `[] = {} ...`, but ChatGPT could explain it quite nicely. It will assign those cells
 * whose index is in the [], and zero-initialise the other cells.
 */
static const rcc_desc_t rcc_table[RCC_PERIPH_COUNT] = {
    [RCC_GPIOA] = { &RCCx->AHB2ENR, RCC_AHB2ENR_GPIOAEN },
    [RCC_GPIOB] = { &RCCx->AHB2ENR, RCC_AHB2ENR_GPIOBEN },
    [RCC_GPIOC] = { &RCCx->AHB2ENR, RCC_AHB2ENR_GPIOCEN },
    [RCC_GPIOD] = { &RCCx->AHB2ENR, RCC_AHB2ENR_GPIODEN },
    [RCC_GPIOE] = { &RCCx->AHB2ENR, RCC_AHB2ENR_GPIOEEN },
    [RCC_GPIOF] = { &RCCx->AHB2ENR, RCC_AHB2ENR_GPIOFEN },
    [RCC_GPIOG] = { &RCCx->AHB2ENR, RCC_AHB2ENR_GPIOGEN },
    [RCC_GPIOH] = { &RCCx->AHB2ENR, RCC_AHB2ENR_GPIOHEN },

    [RCC_LPUART1] = { &RCCx->APB1ENR2, RCC_APB1ENR2_LPUART1EN },
    [RCC_PWR]     = { &RCCx->APB1ENR1, RCC_APB1ENR1_PWREN },
};

/**
 * @brief Enable power to a peripheral
 * 
 */
static void stm32l5_rcc_enable(struct rcc_dev *dev, rcc_periph_t periph) {
#if HAL_SECURE
    (void)dev;

    if (periph >= RCC_PERIPH_COUNT) return;

    const rcc_desc_t *d = &rcc_table[periph];
    if (!d->reg) return; // if I forget to add some peripheral to the table above, the reg entry will be 0. This stops a hardfault from happening
    *d->reg |= d->bit;
    (void)*d->reg;
#endif
}

/**
 * @brief Disable power to a peripheral
 * 
 */
static void stm32l5_rcc_disable(struct rcc_dev *dev, rcc_periph_t periph) {
#if HAL_SECURE
    (void)dev;

    if (periph >= RCC_PERIPH_COUNT) return;

    const rcc_desc_t *d = &rcc_table[periph];
    if (!d->reg) return;
    *d->reg &= ~d->bit;
    (void)*d->reg;
#endif
}

/**
 * @brief Is the power to a peripheral enabled?
 * 
 */
static int stm32l5_rcc_is_enabled(struct rcc_dev *dev, rcc_periph_t periph) {
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

static const rcc_clock_mux_desc_t clock_mux_table[] = {
    [RCC_LPUART1] = {
        .reg       = &RCCx->CCIPR1,
        .mask      = RCC_CCIPR1_LPUART1SEL_Msk,
        .pos       = RCC_CCIPR1_LPUART1SEL_Pos,
        .clock_map = {
            [RCC_PCKL1]  = 0x0U,
            [RCC_SYSCLK] = 0x1U,
            [RCC_HSI16]  = 0x2U,
            [RCC_LSE]    = 0x3U,
        },
    },
    // more
};

void stm32l5_rcc_set_peripheral_clock(struct rcc_dev *dev, rcc_periph_t periph, rcc_periph_clock_source_t clock) {
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
static inline void stm32l5_rcc_set_wait_states(struct flash_dev *flash, uint32_t target_sysclk_hz) {
    uint32_t wait_states;

    if      (target_sysclk_hz <= 20000000)  wait_states = 0;
    else if (target_sysclk_hz <= 40000000)  wait_states = 1;
    else if (target_sysclk_hz <= 60000000)  wait_states = 2;
    else if (target_sysclk_hz <= 80000000)  wait_states = 3;
    else if (target_sysclk_hz <= 100000000) wait_states = 4;
    else                                    wait_states = 5; // this is the CAP though, 110000000. Board cannot run faster

    flash_set_latency(flash, wait_states);
    while(flash_get_latency(flash) != wait_states);
}
#endif

// input for me to get 110 MHz is m = 1, n = 55, div = 7
void stm32l5_configure_pll(struct rcc_dev *dev,
                           struct flash_dev *flash,
                           uint32_t pll_n,
                           uint32_t pll_m,
                           uint32_t pll_div,
                           uint32_t target_sysclk_hz) {
#if HAL_SECURE
//    RCCx->SECCFGR |= (1 << RCC_SECCFGR_PLLSEC_Pos); // come back to this when we enable trustzone for this board

    if (RCCx->CR & RCC_CR_PLLON) {
        RCCx->CR &= ~RCC_CR_PLLON;
        while (RCCx->CR & RCC_CR_PLLRDY);
    }

    stm32l5_rcc_set_wait_states(flash, target_sysclk_hz);

    // Configure MSI clock to be 4MHz (reset value)
    // RCCx->CR |= (0x6U << RCC_CR_MSIRANGE_Pos); // on my board, the rest values all configure 4MHz, so I don't need to do this    
    RCCx->CR |= RCC_CR_MSIRGSEL;
    while (!(RCCx->CR & RCC_CR_MSIRDY));

    RCCx->PLLCFGR = 0;
    RCCx->PLLCFGR |= RCC_PLLCFGR_PLLSRC_0; // Select MSI as clock source for PLL
    RCCx->PLLCFGR |= (  ( (pll_m - 1)   << RCC_PLLCFGR_PLLM_Pos)
                        | (pll_n        << RCC_PLLCFGR_PLLN_Pos)
                        | (pll_div      << RCC_PLLCFGR_PLLPDIV_Pos)
                        );
    RCCx->PLLCFGR |= (  (  1U << RCC_PLLCFGR_PLLPEN_Pos)
                        | (1U << RCC_PLLCFGR_PLLQEN_Pos)
                        | (1U << RCC_PLLCFGR_PLLREN_Pos)
                        );

    // Enable PLL
    RCCx->CR |= RCC_CR_PLLON;
    while (!(RCCx->CR & RCC_CR_PLLRDY));

    // Set PLL as SYSCLK source
    RCCx->CFGR |= (3U << RCC_CFGR_SW_Pos);
    while ((RCCx->CFGR & RCC_CFGR_SWS) != RCC_CFGR_SWS);
#endif
}

static const rcc_driver_api_t stm32l5_rcc_api = {
    .enable = stm32l5_rcc_enable,
    .disable = stm32l5_rcc_disable,
    .is_enabled = stm32l5_rcc_is_enabled,
    .set_peripheral_clock = stm32l5_rcc_set_peripheral_clock,
    .configure_pll = stm32l5_configure_pll,
};

void stm32l5_rcc_create(rcc_dev_t *dev) {
    dev->api = &stm32l5_rcc_api;
    dev->backend = NULL;
}