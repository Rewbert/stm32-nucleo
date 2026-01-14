
#include "hal/platform/clock.h"
#include "stm32l5xx.h"

/**
 * @brief Configure the STM32L552 MCU to run at 110 MHz. This is very platform dependent.
 * It took me ages to figure out how this crap works.
 * 
 */
void platform_clock_configure_110mhz(void) {
#if HAL_SECURE
  RCCx->SECCFGR |= (1 << RCC_SECCFGR_PLLSEC_Pos);                           
  RCCx->CR |= RCC_CR_MSIRGSEL;                                              
  while (!(RCCx->CR & RCC_CR_MSIRDY));                                      
  RCCx->PLLCFGR |= RCC_PLLCFGR_PLLSRC_0;                                    
  RCCx->PLLCFGR |= ( (0U << RCC_PLLCFGR_PLLM_Pos)                           
                   | (55U << RCC_PLLCFGR_PLLN_Pos)                          
                   | (7U << RCC_PLLCFGR_PLLPDIV_Pos)                        
                   );                                                       
  RCCx->PLLCFGR |= ( (1U << RCC_PLLCFGR_PLLPEN_Pos)                         
                   | (1U << RCC_PLLCFGR_PLLQEN_Pos)                         
                   | (1U << RCC_PLLCFGR_PLLREN_Pos)                         
                   );                                                       
  RCCx->CR |= RCC_CR_PLLON;                                                 
  while (!(RCCx->CR & RCC_CR_PLLRDY));                                      
  FLASHx->ACR = (FLASHx->ACR & ~FLASH_ACR_LATENCY) | FLASH_ACR_LATENCY_4WS;
  RCCx->CFGR |= (3U << RCC_CFGR_SW_Pos);                                    
  while ((RCCx->CFGR & RCC_CFGR_SWS) != RCC_CFGR_SWS);
#endif
}