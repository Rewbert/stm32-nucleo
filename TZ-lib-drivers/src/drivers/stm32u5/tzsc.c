#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32u5/tzsc.h"
#include "drivers/tzsc.h"

#include "stm32u5xx.h"

/**
 * @brief U5 has two GTZC instances, each with up to three SECCFGR registers.
 * GTZC1 covers main-bus peripherals (APB1/APB2/AHB2/AHB1).
 * GTZC2 covers low-power peripherals (APB3/AHB3), including LPUART1.
 *
 * tzsc_num: 1 = GTZC_TZSC1, 2 = GTZC_TZSC2
 * reg:      1/2/3 = SECCFGR1/SECCFGR2/SECCFGR3 within the selected TZSC
 */
typedef struct {
    uint8_t  tzsc_num;
    uint8_t  reg;
    uint32_t mask;
} periph_info_t;

static const periph_info_t periph_table[] = {
    /* ---- GTZC1 SECCFGR1 ---- */
    [TZSC_PERIPH_TIM2]    = { 1, 1, GTZC_TZSC1_SECCFGR1_TIM2_Msk    },
    [TZSC_PERIPH_TIM3]    = { 1, 1, GTZC_TZSC1_SECCFGR1_TIM3_Msk    },
    [TZSC_PERIPH_TIM4]    = { 1, 1, GTZC_TZSC1_SECCFGR1_TIM4_Msk    },
    [TZSC_PERIPH_TIM5]    = { 1, 1, GTZC_TZSC1_SECCFGR1_TIM5_Msk    },
    [TZSC_PERIPH_TIM6]    = { 1, 1, GTZC_TZSC1_SECCFGR1_TIM6_Msk    },
    [TZSC_PERIPH_TIM7]    = { 1, 1, GTZC_TZSC1_SECCFGR1_TIM7_Msk    },
    [TZSC_PERIPH_WWDG]    = { 1, 1, GTZC_TZSC1_SECCFGR1_WWDG_Msk    },
    [TZSC_PERIPH_IWDG]    = { 1, 1, GTZC_TZSC1_SECCFGR1_IWDG_Msk    },
    [TZSC_PERIPH_SPI2]    = { 1, 1, GTZC_TZSC1_SECCFGR1_SPI2_Msk    },
    [TZSC_PERIPH_USART2]  = { 1, 1, GTZC_TZSC1_SECCFGR1_USART2_Msk  },
    [TZSC_PERIPH_USART3]  = { 1, 1, GTZC_TZSC1_SECCFGR1_USART3_Msk  },
    [TZSC_PERIPH_UART4]   = { 1, 1, GTZC_TZSC1_SECCFGR1_UART4_Msk   },
    [TZSC_PERIPH_UART5]   = { 1, 1, GTZC_TZSC1_SECCFGR1_UART5_Msk   },
    [TZSC_PERIPH_I2C1]    = { 1, 1, GTZC_TZSC1_SECCFGR1_I2C1_Msk    },
    [TZSC_PERIPH_I2C2]    = { 1, 1, GTZC_TZSC1_SECCFGR1_I2C2_Msk    },
    [TZSC_PERIPH_CRS]     = { 1, 1, GTZC_TZSC1_SECCFGR1_CRS_Msk     },
    [TZSC_PERIPH_I2C4]    = { 1, 1, GTZC_TZSC1_SECCFGR1_I2C4_Msk    },
    [TZSC_PERIPH_LPTIM2]  = { 1, 1, GTZC_TZSC1_SECCFGR1_LPTIM2_Msk  },
    [TZSC_PERIPH_FDCAN1]  = { 1, 1, GTZC_TZSC1_SECCFGR1_FDCAN1_Msk  },
    [TZSC_PERIPH_UCPD1]   = { 1, 1, GTZC_TZSC1_SECCFGR1_UCPD1_Msk   },
    /* ---- GTZC1 SECCFGR2 ---- */
    [TZSC_PERIPH_TIM1]    = { 1, 2, GTZC_TZSC1_SECCFGR2_TIM1_Msk    },
    [TZSC_PERIPH_SPI1]    = { 1, 2, GTZC_TZSC1_SECCFGR2_SPI1_Msk    },
    [TZSC_PERIPH_TIM8]    = { 1, 2, GTZC_TZSC1_SECCFGR2_TIM8_Msk    },
    [TZSC_PERIPH_USART1]  = { 1, 2, GTZC_TZSC1_SECCFGR2_USART1_Msk  },
    [TZSC_PERIPH_TIM15]   = { 1, 2, GTZC_TZSC1_SECCFGR2_TIM15_Msk   },
    [TZSC_PERIPH_TIM16]   = { 1, 2, GTZC_TZSC1_SECCFGR2_TIM16_Msk   },
    /* ---- GTZC2 SECCFGR1 ---- (low-power peripherals on APB3) */
    [TZSC_PERIPH_LPUART1] = { 2, 1, GTZC_TZSC2_SECCFGR1_LPUART1_Msk },
    [TZSC_PERIPH_I2C3]    = { 2, 1, GTZC_TZSC2_SECCFGR1_I2C3_Msk    },
    [TZSC_PERIPH_LPTIM1]  = { 2, 1, GTZC_TZSC2_SECCFGR1_LPTIM1_Msk  },
    [TZSC_PERIPH_LPTIM3]  = { 2, 1, GTZC_TZSC2_SECCFGR1_LPTIM3_Msk  },
    [TZSC_PERIPH_OPAMP]   = { 2, 1, GTZC_TZSC2_SECCFGR1_OPAMP_Msk   },
    [TZSC_PERIPH_COMP]    = { 2, 1, GTZC_TZSC2_SECCFGR1_COMP_Msk     },
    [TZSC_PERIPH_VREFBUF] = { 2, 1, GTZC_TZSC2_SECCFGR1_VREFBUF_Msk },
};

static void stm32u5_tzsc_set_periph(struct tzsc_dev *dev, tzsc_periph_t periph, tzsc_security_t sec) {
#if HAL_SECURE
    stm32u5_tzsc_backend_t *backend = (stm32u5_tzsc_backend_t *) dev->backend;

    if (periph >= (tzsc_periph_t)(sizeof(periph_table) / sizeof(periph_table[0]))) return;

    const periph_info_t *info = &periph_table[periph];
    if (!info->mask) return; /* unimplemented peripheral */

    GTZC_TZSC_TypeDef *tzsc = (info->tzsc_num == 2) ? backend->tzsc2 : backend->tzsc1;

    volatile uint32_t *reg;
    switch (info->reg) {
        case 1: reg = &tzsc->SECCFGR1; break;
        case 2: reg = &tzsc->SECCFGR2; break;
        case 3: reg = &tzsc->SECCFGR3; break;
        default: return;
    }

    switch (sec) {
        case TZSC_SECURE:    *reg |=  info->mask; return;
        case TZSC_NONSECURE: *reg &= ~info->mask; return;
        default: return;
    }
#endif
}

static void stm32u5_tzsc_lock(struct tzsc_dev *dev) {
#if HAL_SECURE
    stm32u5_tzsc_backend_t *backend = (stm32u5_tzsc_backend_t *) dev->backend;
    backend->tzsc1->CR |= GTZC_TZSC_CR_LCK_Msk;
    backend->tzsc2->CR |= GTZC_TZSC_CR_LCK_Msk;
#endif
}

static const tzsc_driver_api_t stm32u5_tzsc_api = {
    .set_periph = stm32u5_tzsc_set_periph,
    .lock       = stm32u5_tzsc_lock,
};

void stm32u5_tzsc_create(tzsc_dev_t *dev, stm32u5_tzsc_backend_t *backend) {
#if HAL_SECURE
    backend->tzsc1 = GTZC_TZSC1_S;
    backend->tzsc2 = GTZC_TZSC2_S;
#endif

    // we want to be able to initialize this on the nonsecure side as well such that
    // the API is there, even if the API functions never do anything on the nonsecure side.
    dev->api     = &stm32u5_tzsc_api;
    dev->backend = backend;
}
