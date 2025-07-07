/*

By default, MSI RC oscillator configured at 4 MHz is used after reset. We wish to use a better clock.

We configure the PLL to generate a 110 MHz clock.

*/

#include "stm32l5xx.h"

void configure_clock(void) {
    RCC->CR |= RCC_CR_MSIRGSEL; // By setting this, MSI will pick MSIRANGE from the MSIRANGE register
    while (!(RCC->CR & RCC_CR_MSIRDY));

    // Configure PLL to use MSI as input (MSI is the clock that is on by default)
    RCC->PLLCFGR |= RCC_PLLCFGR_PLLSRC_0;

    // set dividers and multipliers value
    RCC->PLLCFGR |= ( (0U  << RCC_PLLCFGR_PLLM_Pos)
                    | (55U << RCC_PLLCFGR_PLLN_Pos) // multiply incoming 4MHz by 55, yielding 220. PLLR will always divide by 2 before feeding it as output, resulting in 110 MHz (the stated max for my board)
                    | (7U  << RCC_PLLCFGR_PLLPDIV_Pos)
                    );

    // turn on the PLL outputs
    RCC->PLLCFGR |= ( (1U << RCC_PLLCFGR_PLLPEN_Pos)
                    | (1U << RCC_PLLCFGR_PLLQEN_Pos)
                    | (1U << RCC_PLLCFGR_PLLREN_Pos) // tjis one is fed as sysclk output
                    );

    // // Enable the PLL, this has to happen after everything else
    RCC->CR |= RCC_CR_PLLON;
    while (!(RCC->CR & RCC_CR_PLLRDY));

    // Configure Flash wait states for 110 MHz
    FLASH->ACR = (FLASH->ACR & ~FLASH_ACR_LATENCY) | FLASH_ACR_LATENCY_4WS; // chatGPT helped me learn that I had to do this. If I do this as the last thing I do, it doesn't work. It has to be before the system clock is set to PLL.
                                                                            // apparently the flash is slower than what the (now) 110 MHz clock can handle, and this configuration makes the CPU wait enough time before it reads from flash.
                                                                            // Otherwise it might read the next instruction before it is ready, and then execute garbage

    // // Set PLL as system clock source
    RCC->CFGR = (3U << RCC_CFGR_SW_Pos);
    while ((RCC->CFGR & RCC_CFGR_SWS) != RCC_CFGR_SWS);
}