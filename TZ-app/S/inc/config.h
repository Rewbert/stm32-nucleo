#ifndef CONFIG_H
#define CONFIG_H

#define ENABLE_GPIO_PORT(port)                               \
  {                                                          \
    volatile uint32_t dummy;                                 \
    RCC_S->AHB2ENR |= (1 << RCC_AHB2ENR_GPIO##port##EN_Pos); \
    dummy = RCC_S->AHB2ENR;                                  \
    dummy = RCC_S->AHB2ENR;                                  \
  }

#define INPUT              0x00
#define OUTPUT             0x01
#define ALTERNATE_FUNCTION 0x10
#define ANALOG             0x11

#define SET_GPIO_MODE(port, pin, mode)                    \
  GPIO##port##_S->MODER &= ~(GPIO_MODER_MODE##pin##_Msk); \
  GPIO##port##_S->MODER |= (mode << GPIO_MODER_MODE##pin##_Pos);

#define CONFIGURE_AS_LED(port, pin)    SET_GPIO_MODE(port, pin, OUTPUT)
#define CONFIGURE_AS_BUTTON(port, pin) SET_GPIO_MODE(port, pin, INPUT)

#define MAKE_GPIO_NONSECURE(port, pin) GPIO##port##_S->SECCFGR &= ~(1 << pin)
#define MAKE_GPIO_SECURE(port, pin)    GPIO##port##_S->SECCFGR &= ~(0 << pin)

#define CONFIGURE_BUTTON_0(port) CONFIGURE_BUTTON_CR(1, port, 0)
#define CONFIGURE_BUTTON_1(port) CONFIGURE_BUTTON_CR(1, port, 1)
#define CONFIGURE_BUTTON_2(port) CONFIGURE_BUTTON_CR(1, port, 2)
#define CONFIGURE_BUTTON_3(port) CONFIGURE_BUTTON_CR(1, port, 3)
#define CONFIGURE_BUTTON_4(port) CONFIGURE_BUTTON_CR(2, port, 4)
#define CONFIGURE_BUTTON_5(port) CONFIGURE_BUTTON_CR(2, port, 5)
#define CONFIGURE_BUTTON_6(port) CONFIGURE_BUTTON_CR(2, port, 6)
#define CONFIGURE_BUTTON_7(port) CONFIGURE_BUTTON_CR(2, port, 7)
#define CONFIGURE_BUTTON_8(port) CONFIGURE_BUTTON_CR(3, port, 8)
#define CONFIGURE_BUTTON_9(port) CONFIGURE_BUTTON_CR(3, port, 9)
#define CONFIGURE_BUTTON_10(port) CONFIGURE_BUTTON_CR(3, port, 10)
#define CONFIGURE_BUTTON_11(port) CONFIGURE_BUTTON_CR(3, port, 11)
#define CONFIGURE_BUTTON_12(port) CONFIGURE_BUTTON_CR(4, port, 12)
#define CONFIGURE_BUTTON_13(port) CONFIGURE_BUTTON_CR(4, port, 13)
#define CONFIGURE_BUTTON_14(port) CONFIGURE_BUTTON_CR(4, port, 14)
#define CONFIGURE_BUTTON_15(port) CONFIGURE_BUTTON_CR(4, port, 15)

#define CONFIGURE_BUTTON(port, pin) CONFIGURE_BUTTON_##pin(port)

#define CONFIGURE_BUTTON_CR(cr, port, pin)                              \
  GPIO##port##_S->MODER  &= ~(GPIO_MODER_MODE##pin##_Msk);              \
  GPIO##port##_S->PUPDR  &= ~(GPIO_PUPDR_PUPD##pin##_Msk);              \
  GPIO##port##_S->PUPDR  |= (1 << GPIO_PUPDR_PUPD##pin##_Pos);          \
  EXTI_S->EXTICR[cr - 1] &= ~(EXTI_EXTICR##cr##_EXTI##pin##_Msk);       \
  EXTI_S->EXTICR[cr - 1] |= (0x0 << EXTI_EXTICR##cr##_EXTI##pin##_Pos); \
  EXTI_S->IMR1           |= (1 << EXTI_IMR1_IM##pin##_Pos);             \
  EXTI_S->FTSR1          |= (1 << EXTI_FTSR1_FT##pin##_Pos);            \
  EXTI_S->SECCFGR1       |= (1 << EXTI_SECCFGR1_SEC##pin##_Pos);        \
  EXTI_S->PRIVCFGR1      |= (1 << EXTI_PRIVCFGR1_PRIV##pin##_Pos);      \
  NVIC_SetPriority(EXTI##pin##_IRQn, 2);                                \
  NVIC_EnableIRQ(EXTI##pin##_IRQn);

#define CONFIGURE_NONSECURE_BUTTON(port, pin) \
  ENABLE_GPIO_PORT(port); \
  MAKE_GPIO_NONSECURE(port, pin); \
  NVIC_SetTargetState(EXTI##pin##_IRQn);

#define CONFIGURE_SECURE_BUTTON(port, pin) \
  ENABLE_GPIO_PORT(port); \
  CONFIGURE_BUTTON(port, pin);

#define CONFIGURE_SECURE_LED(port, pin) \
  ENABLE_GPIO_PORT(port); \
  CONFIGURE_AS_LED(port, pin); 

#define CONFIGURE_NONSECURE_LED(port, pin) \
  CONFIGURE_SECURE_LED(port, pin); \
  MAKE_GPIO_NONSECURE(port, pin);

#define TOGGLE_LED(port, pin) \
  GPIO##port##_NS->ODR ^= (1 << pin);

#define CONFIGURE_CLOCK_110_MHZ()                                             \
  RCC_S->SECCFGR |= (1 << RCC_SECCFGR_PLLSEC_Pos);                            \
  RCC_S->CR |= RCC_CR_MSIRGSEL;                                               \
  while (!(RCC_S->CR & RCC_CR_MSIRDY));                                       \
  RCC_S->PLLCFGR |= RCC_PLLCFGR_PLLSRC_0;                                     \
  RCC_S->PLLCFGR |= ( (0U << RCC_PLLCFGR_PLLM_Pos)                            \
                    | (55U << RCC_PLLCFGR_PLLN_Pos)                           \
                    | (7U << RCC_PLLCFGR_PLLPDIV_Pos)                         \
                    );                                                        \
  RCC_S->PLLCFGR |= ( (1U << RCC_PLLCFGR_PLLPEN_Pos)                          \
                    | (1U << RCC_PLLCFGR_PLLQEN_Pos)                          \
                    | (1U << RCC_PLLCFGR_PLLREN_Pos)                          \
                    );                                                        \
  RCC_S->CR |= RCC_CR_PLLON;                                                  \
  while (!(RCC_S->CR & RCC_CR_PLLRDY));                                       \
  FLASH_S->ACR = (FLASH_S->ACR & ~FLASH_ACR_LATENCY) | FLASH_ACR_LATENCY_4WS; \
  RCC_S->CFGR |= (3U << RCC_CFGR_SW_Pos);                                     \
  while ((RCC_S->CFGR & RCC_CFGR_SWS) != RCC_CFGR_SWS);

#endif /* CONFIG_H */