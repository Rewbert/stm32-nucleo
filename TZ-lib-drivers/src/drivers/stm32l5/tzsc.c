#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32l5/tzsc.h"
#include "drivers/tzsc.h"

#include "stm32l5xx.h"

/**
 * @brief Maps each tzsc_periph_t to the register (1 or 2) and the bit mask within it.
 * 
 */
typedef struct {
    uint8_t  reg;
    uint32_t mask;
} periph_info_t;

static const periph_info_t periph_table[] = {
    /* SECCFGR1 */
    [TZSC_PERIPH_TIM2]        = { 1, GTZC_CFGR1_TIM2_Msk    },
    [TZSC_PERIPH_TIM3]        = { 1, GTZC_CFGR1_TIM3_Msk    },
    [TZSC_PERIPH_TIM4]        = { 1, GTZC_CFGR1_TIM4_Msk    },
    [TZSC_PERIPH_TIM5]        = { 1, GTZC_CFGR1_TIM5_Msk    },
    [TZSC_PERIPH_TIM6]        = { 1, GTZC_CFGR1_TIM6_Msk    },
    [TZSC_PERIPH_TIM7]        = { 1, GTZC_CFGR1_TIM7_Msk    },
    [TZSC_PERIPH_WWDG]        = { 1, GTZC_CFGR1_WWDG_Msk    },
    [TZSC_PERIPH_IWDG]        = { 1, GTZC_CFGR1_IWDG_Msk    },
    [TZSC_PERIPH_SPI2]        = { 1, GTZC_CFGR1_SPI2_Msk    },
    [TZSC_PERIPH_SPI3]        = { 1, GTZC_CFGR1_SPI3_Msk    },
    [TZSC_PERIPH_USART2]      = { 1, GTZC_CFGR1_USART2_Msk  },
    [TZSC_PERIPH_USART3]      = { 1, GTZC_CFGR1_USART3_Msk  },
    [TZSC_PERIPH_UART4]       = { 1, GTZC_CFGR1_UART4_Msk   },
    [TZSC_PERIPH_UART5]       = { 1, GTZC_CFGR1_UART5_Msk   },
    [TZSC_PERIPH_I2C1]        = { 1, GTZC_CFGR1_I2C1_Msk    },
    [TZSC_PERIPH_I2C2]        = { 1, GTZC_CFGR1_I2C2_Msk    },
    [TZSC_PERIPH_I2C3]        = { 1, GTZC_CFGR1_I2C3_Msk    },
    [TZSC_PERIPH_CRS]         = { 1, GTZC_CFGR1_CRS_Msk     },
    [TZSC_PERIPH_DAC1]        = { 1, GTZC_CFGR1_DAC1_Msk    },
    [TZSC_PERIPH_OPAMP]       = { 1, GTZC_CFGR1_OPAMP_Msk   },
    [TZSC_PERIPH_LPTIM1]      = { 1, GTZC_CFGR1_LPTIM1_Msk  },
    [TZSC_PERIPH_LPUART1]     = { 1, GTZC_CFGR1_LPUART1_Msk },
    [TZSC_PERIPH_I2C4]        = { 1, GTZC_CFGR1_I2C4_Msk    },
    [TZSC_PERIPH_LPTIM2]      = { 1, GTZC_CFGR1_LPTIM2_Msk  },
    [TZSC_PERIPH_LPTIM3]      = { 1, GTZC_CFGR1_LPTIM3_Msk  },
    [TZSC_PERIPH_FDCAN1]      = { 1, GTZC_CFGR1_FDCAN1_Msk  },
    [TZSC_PERIPH_USBFS]       = { 1, GTZC_CFGR1_USBFS_Msk   },
    [TZSC_PERIPH_UCPD1]       = { 1, GTZC_CFGR1_UCPD1_Msk   },
    [TZSC_PERIPH_VREFBUF]     = { 1, GTZC_CFGR1_VREFBUF_Msk },
    [TZSC_PERIPH_COMP]        = { 1, GTZC_CFGR1_COMP_Msk    },
    [TZSC_PERIPH_TIM1]        = { 1, GTZC_CFGR1_TIM1_Msk    },
    [TZSC_PERIPH_SPI1]        = { 1, GTZC_CFGR1_SPI1_Msk    },
    /* SECCFGR2 */
    [TZSC_PERIPH_TIM8]        = { 2, GTZC_CFGR2_TIM8_Msk        },
    [TZSC_PERIPH_USART1]      = { 2, GTZC_CFGR2_USART1_Msk      },
    [TZSC_PERIPH_TIM15]       = { 2, GTZC_CFGR2_TIM15_Msk       },
    [TZSC_PERIPH_TIM16]       = { 2, GTZC_CFGR2_TIM16_Msk       },
    [TZSC_PERIPH_TIM17]       = { 2, GTZC_CFGR2_TIM17_Msk       },
    [TZSC_PERIPH_SAI1]        = { 2, GTZC_CFGR2_SAI1_Msk        },
    [TZSC_PERIPH_SAI2]        = { 2, GTZC_CFGR2_SAI2_Msk        },
    [TZSC_PERIPH_DFSDM1]      = { 2, GTZC_CFGR2_DFSDM1_Msk      },
    [TZSC_PERIPH_CRC]         = { 2, GTZC_CFGR2_CRC_Msk         },
    [TZSC_PERIPH_TSC]         = { 2, GTZC_CFGR2_TSC_Msk         },
    [TZSC_PERIPH_ICACHE_REG]  = { 2, GTZC_CFGR2_ICACHE_REG_Msk  },
    [TZSC_PERIPH_ADC]         = { 2, GTZC_CFGR2_ADC_Msk         },
    [TZSC_PERIPH_HASH]        = { 2, GTZC_CFGR2_HASH_Msk        },
    [TZSC_PERIPH_RNG]         = { 2, GTZC_CFGR2_RNG_Msk         },
    [TZSC_PERIPH_SDMMC1]      = { 2, GTZC_CFGR2_SDMMC1_Msk      },
    [TZSC_PERIPH_FMC_REG]     = { 2, GTZC_CFGR2_FMC_REG_Msk     },
    [TZSC_PERIPH_OCTOSPI1_REG]= { 2, GTZC_CFGR2_OCTOSPI1_REG_Msk},
};

static void stm32l5_tzsc_set_periph(struct tzsc_dev *dev, tzsc_periph_t periph, tzsc_security_t sec) {
#if HAL_SECURE
    stm32l5_tzsc_backend_t *backend = (stm32l5_tzsc_backend_t *) dev->backend;
    const periph_info_t *info = &periph_table[periph];

    volatile uint32_t *reg = (info->reg == 1) ? &backend->tzsc->SECCFGR1 : &backend->tzsc->SECCFGR2;

    switch (sec) {
        case TZSC_SECURE:    *reg |=  info->mask; return;
        case TZSC_NONSECURE: *reg &= ~info->mask; return;
        default: return;
    }
#endif
}

static void stm32l5_tzsc_lock(struct tzsc_dev *dev) {
#if HAL_SECURE
    stm32l5_tzsc_backend_t *backend = (stm32l5_tzsc_backend_t *) dev->backend;
    backend->tzsc->CR |= GTZC_TZSC_CR_LCK_Msk;
#endif
}

static const tzsc_driver_api_t stm32l5_tzsc_api = {
    .set_periph = stm32l5_tzsc_set_periph,
    .lock       = stm32l5_tzsc_lock,
};

void stm32l5_tzsc_create(tzsc_dev_t *dev, stm32l5_tzsc_backend_t *backend) {
#if HAL_SECURE
    backend->tzsc = GTZC_TZSC;

    dev->api     = &stm32l5_tzsc_api;
    dev->backend = backend;
#endif
}
