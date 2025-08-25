#ifndef CONFIG_H
#define CONFIG_H

#define ENABLE_GPIO_PORT_BASIC(port)                         \
  {                                                          \
    volatile uint32_t dummy;                                 \
    RCC_S->AHB2ENR |= (1 << RCC_AHB2ENR_GPIO##port##EN_Pos); \
    dummy = RCC_S->AHB2ENR;                                  \
    dummy = RCC_S->AHB2ENR;                                  \
  }

#define ENABLE_GPIO_PORT_A() ENABLE_GPIO_PORT_BASIC(A)
#define ENABLE_GPIO_PORT_B() ENABLE_GPIO_PORT_BASIC(B)
#define ENABLE_GPIO_PORT_C() ENABLE_GPIO_PORT_BASIC(C)
#define ENABLE_GPIO_PORT_D() ENABLE_GPIO_PORT_BASIC(D)
#define ENABLE_GPIO_PORT_E() ENABLE_GPIO_PORT_BASIC(E)
#define ENABLE_GPIO_PORT_F() ENABLE_GPIO_PORT_BASIC(F)
#define ENABLE_GPIO_PORT_G()         \
  RCC_S->APB1ENR1 |= RCC_APB1ENR1_PWREN; \
  PWR_S->CR2      |= PWR_CR2_IOSV;       \
  ENABLE_GPIO_PORT_BASIC(G);
#define ENABLE_GPIO_PORT_H() ENABLE_GPIO_PORT_BASIC(H)

#define ENABLE_GPIO_PORT(port) ENABLE_GPIO_PORT_##port()

#define INPUT              0U
#define OUTPUT             1U
#define ALTERNATE_FUNCTION 2U
#define ANALOG             3U

#define SET_GPIO_MODE(port, pin, mode)                    \
  GPIO##port##_S->MODER &= ~(GPIO_MODER_MODE##pin##_Msk); \
  GPIO##port##_S->MODER |= (mode << GPIO_MODER_MODE##pin##_Pos);

#define NO_PUPD   0
#define PULL_UP   1
#define PULL_DOWN 2

#define SET_GPIO_PUPDR(port, pin, mode)                        \
  GPIO##port##_S->PUPDR &= ~(2 << GPIO_PUPDR_PUPD##pin##_Pos); \
  GPIO##port##_S->PUPDR |= (mode << GPIO_PUPDR_PUPD##pin##_Pos);

#define AF0  0x0
#define AF1  0x1
#define AF2  0x2
#define AF3  0x3
#define AF4  0x4
#define AF5  0x5
#define AF6  0x6
#define AF7  0x7
#define AF8  0x8
#define AF9  0x9
#define AF10 0xA
#define AF11 0xB
#define AF12 0xC
#define AF13 0xD
#define AF14 0xE
#define AF15 0xF

#define SET_ALTERNATE_FUNCTION_L(port, pin, function) \
  GPIO##port##_S->AFR[0] &= ~(GPIO_AFRL_AFSEL##pin); \
  GPIO##port##_S->AFR[0] |= (function << GPIO_AFRL_AFSEL##pin##_Pos);

#define SET_ALTERNATE_FUNCTION_H(port, pin, function) \
  GPIO##port##_S->AFR[1] &= ~(GPIO_AFRH_AFSEL##pin); \
  GPIO##port##_S->AFR[1] |= (function << GPIO_AFRH_AFSEL##pin##_Pos);

#define SET_ALTERNATE_FUNCTION_0(port, function) SET_ALTERNATE_FUNCTION_L(port, 0, function)
#define SET_ALTERNATE_FUNCTION_1(port, function) SET_ALTERNATE_FUNCTION_L(port, 1, function)
#define SET_ALTERNATE_FUNCTION_2(port, function) SET_ALTERNATE_FUNCTION_L(port, 2, function)
#define SET_ALTERNATE_FUNCTION_3(port, function) SET_ALTERNATE_FUNCTION_L(port, 3, function)
#define SET_ALTERNATE_FUNCTION_4(port, function) SET_ALTERNATE_FUNCTION_L(port, 4, function)
#define SET_ALTERNATE_FUNCTION_5(port, function) SET_ALTERNATE_FUNCTION_L(port, 5, function)
#define SET_ALTERNATE_FUNCTION_6(port, function) SET_ALTERNATE_FUNCTION_L(port, 6, function)
#define SET_ALTERNATE_FUNCTION_7(port, function) SET_ALTERNATE_FUNCTION_L(port, 7, function)

#define SET_ALTERNATE_FUNCTION_8(port, function)  SET_ALTERNATE_FUNCTION_H(port, 8,  function)
#define SET_ALTERNATE_FUNCTION_9(port, function)  SET_ALTERNATE_FUNCTION_H(port, 9,  function)
#define SET_ALTERNATE_FUNCTION_10(port, function) SET_ALTERNATE_FUNCTION_H(port, 10, function)
#define SET_ALTERNATE_FUNCTION_11(port, function) SET_ALTERNATE_FUNCTION_H(port, 11, function)
#define SET_ALTERNATE_FUNCTION_12(port, function) SET_ALTERNATE_FUNCTION_H(port, 12, function)
#define SET_ALTERNATE_FUNCTION_13(port, function) SET_ALTERNATE_FUNCTION_H(port, 13, function)
#define SET_ALTERNATE_FUNCTION_14(port, function) SET_ALTERNATE_FUNCTION_H(port, 14, function)
#define SET_ALTERNATE_FUNCTION_15(port, function) SET_ALTERNATE_FUNCTION_H(port, 15, function)

#define SET_GPIO_ALTERNATE_FUNCTION(port, pin, function) SET_ALTERNATE_FUNCTION_##pin(port, function)

#define CONFIGURE_AS_LED(port, pin)    SET_GPIO_MODE(port, pin, OUTPUT)
#define CONFIGURE_AS_BUTTON(port, pin) SET_GPIO_MODE(port, pin, INPUT)

#define MAKE_GPIO_NONSECURE(port, pin) GPIO##port##_S->SECCFGR &= ~(1 << pin)
#define MAKE_GPIO_SECURE(port, pin)    GPIO##port##_S->SECCFGR &= ~(0 << pin)

// Only 'works' for low-numbered EXTI's. For higher numbers, modify this to
// conditionally write to sec/privcfgr 1 & 2
#define MAKE_EXTI_SECURE(pin) \
  EXTI_S->SECCFGR1 |= (1 << EXTI_SECCFGR1_SEC##pin##_Pos); \
  EXTI_S->PRIVCFGR1 |= (1 << EXTI_PRIVCFGR1_PRIV##pin##_Pos);

#define FALLING
#define RISING
#define BOTH

#define EXTI_TRIGGER_ON_FALLING(pin) EXTI_S->FTSR1 |= (1 << EXTI_FTSR1_FT##pin##_Pos);
#define EXTI_TRIGGER_ON_RISING(pin)  EXTI_S->RTSR1 |= (1 << EXTI_RTSR1_RT##pin##_Pos);
#define EXTI_TRIGGER_ON_BOTH(pin) \
  EXTI_TRIGGER_ON_FALLING(pin); \
  EXTI_TRIGGER_ON_RISING(pin);

#define EXTI_TRIGGER_ON(pin, edge) EXTI_TRIGGER_ON_##edge(pin)

#define EXTI_UNMASK_INTERRUPTS(pin) EXTI_S->IMR1 |= (1 << EXTI_IMR1_IM##pin##_Pos);

#define EXTI_PORTCODE_A 0x00
#define EXTI_PORTCODE_B 0x01
#define EXTI_PORTCODE_C 0x02
#define EXTI_PORTCODE_D 0x03
#define EXTI_PORTCODE_E 0x04
#define EXTI_PORTCODE_F 0x05
#define EXTI_PORTCODE_G 0x06
#define EXTI_PORTCODE_H 0x07

#define EXTI_ROUTE_CR_PIN(cr, pin, portcode) \
  EXTI_S->EXTICR[cr - 1] &= ~(EXTI_EXTICR##cr##_EXTI##pin##_Msk); \
  EXTI_S->EXTICR[cr - 1] |= (portcode << EXTI_EXTICR##cr##_EXTI##pin##_Pos);

#define EXTI_ROUTE_GPIO_0(port) EXTI_ROUTE_CR_PIN(1, 0, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_1(port) EXTI_ROUTE_CR_PIN(1, 1, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_2(port) EXTI_ROUTE_CR_PIN(1, 2, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_3(port) EXTI_ROUTE_CR_PIN(1, 3, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_4(port) EXTI_ROUTE_CR_PIN(2, 4, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_5(port) EXTI_ROUTE_CR_PIN(2, 5, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_6(port) EXTI_ROUTE_CR_PIN(2, 6, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_7(port) EXTI_ROUTE_CR_PIN(2, 7, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_8(port) EXTI_ROUTE_CR_PIN(3, 8, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_9(port) EXTI_ROUTE_CR_PIN(3, 9, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_10(port) EXTI_ROUTE_CR_PIN(3, 10, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_11(port) EXTI_ROUTE_CR_PIN(3, 11, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_12(port) EXTI_ROUTE_CR_PIN(4, 12, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_13(port) EXTI_ROUTE_CR_PIN(4, 13, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_14(port) EXTI_ROUTE_CR_PIN(4, 14, EXTI_PORTCODE_##port)
#define EXTI_ROUTE_GPIO_15(port) EXTI_ROUTE_CR_PIN(4, 15, EXTI_PORTCODE_##port)

#define EXTI_ROUTE_GPIO(port, pin) EXTI_ROUTE_GPIO_##pin(port)

#define NVIC_CONFIGURE_EXTI_IRQ(pin, priority)  \
  NVIC_SetPriority(EXTI##pin##_IRQn, priority); \
  NVIC_EnableIRQ(EXTI##pin##_IRQn);

#define CONFIGURE_BUTTON(port, pin)      \
  SET_GPIO_MODE(port, pin, INPUT);       \
  SET_GPIO_PUPDR(port, pin, PULL_UP);    \
  EXTI_ROUTE_GPIO(port, pin);            \
  EXTI_UNMASK_INTERRUPTS(pin);           \
  EXTI_TRIGGER_ON(pin, FALLING);         \
  NVIC_CONFIGURE_EXTI_IRQ(pin, 2);

#define CONFIGURE_NONSECURE_BUTTON(port, pin) \
  ENABLE_GPIO_PORT(port); \
  MAKE_GPIO_NONSECURE(port, pin); \
  NVIC_SetTargetState(EXTI##pin##_IRQn);

#define CONFIGURE_SECURE_BUTTON(port, pin) \
  ENABLE_GPIO_PORT(port); \
  CONFIGURE_BUTTON(port, pin); \
  MAKE_EXTI_SECURE(pin);

#define CONFIGURE_SECURE_LED(port, pin) \
  ENABLE_GPIO_PORT(port); \
  CONFIGURE_AS_LED(port, pin); 

#define CONFIGURE_NONSECURE_LED(port, pin) \
  CONFIGURE_SECURE_LED(port, pin); \
  MAKE_GPIO_NONSECURE(port, pin);

#define TOGGLE_LED(port, pin) \
  GPIO##port##_S->ODR ^= (1 << pin);

#define TURN_ON_LED(port, pin) \
  GPIO##port##_S->ODR |= (1 << pin);

#define TURN_OFF_LED(port, pin) \
  GPIO##port##_S->ODR &= ~(1 << pin);

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

#define SET_LPUART1_BRR(brr) \
  { volatile uint32_t dummy; \
    LPUART1_S->BRR = 244444; \
    dummy = RCC_S->AHB2ENR;  \
    dummy = RCC_S->AHB2ENR;  \
  }

#define ENABLE_LPUART1_POWER()                              \
  { volatile uint32_t dummy;                                \
    RCC_S->APB1ENR2 |= (0x1 << RCC_APB1ENR2_LPUART1EN_Pos); \
    dummy = RCC_S->APB1ENR2;                                \
    dummy = RCC_S->APB1ENR2;                                \
  }

#define PCLK1  0
#define SYSCLK 1
#define HSI16  2
#define LSE    3

#define SELECT_LPUART1_CLOCK_SOURCE(source)           \
  RCC_S->CCIPR1 &= ~(3 << RCC_CCIPR1_LPUART1SEL_Pos); \
  RCC_S->CCIPR1 |= (source << RCC_CCIPR1_LPUART1SEL_Pos);

#define ACTIVATE_LPUART1() LPUART1_S->CR1 |= USART_CR1_UE | USART_CR1_TE | USART_CR1_RE;  

#define ENABLE_LPUART1()                   \
  SELECT_LPUART1_CLOCK_SOURCE(SYSCLK);     \
  ENABLE_LPUART1_POWER();                  \
  ENABLE_GPIO_PORT(G);                     \
  SET_GPIO_MODE(G, 7, ALTERNATE_FUNCTION); \
  SET_GPIO_MODE(G, 8, ALTERNATE_FUNCTION); \
  SET_GPIO_PUPDR(G, 7, PULL_UP);           \
  SET_GPIO_PUPDR(G, 8, PULL_UP);           \
  SET_GPIO_ALTERNATE_FUNCTION(G, 7, AF8);  \
  SET_GPIO_ALTERNATE_FUNCTION(G, 8, AF8);  \
  SET_LPUART1_BRR(244444);                 \
  ACTIVATE_LPUART1();

#define MAKE_LPUART1_SECURE()                                        \
  GTZC_TZSC_S->SECCFGR1 |= (1 << GTZC_TZSC_SECCFGR1_LPUART1SEC_Pos); \
  GTZC_TZSC_S->PRIVCFGR1 |= (1 << GTZC_TZSC_PRIVCFGR1_LPUART1PRIV_Pos);
  // I've left out the register configurations for capturing illegal access event interrupts.
  // Relevant registers are IER1, SR1, FCR1

#define ENABLE_SECURE_LPUART1() \
  ENABLE_LPUART1();             \
  MAKE_LPUART1_SECURE();

#endif /* CONFIG_H */