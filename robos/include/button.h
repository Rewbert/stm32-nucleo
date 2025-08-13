#ifndef BUTTON_H
#define BUTTON_H

#define ENABLE_GPIO_PORT_BASIC(port)                         \
  {                                                          \
    volatile uint32_t dummy;                                 \
    RCC_NS->AHB2ENR |= (1 << RCC_AHB2ENR_GPIO##port##EN_Pos); \
    dummy = RCC_NS->AHB2ENR;                                  \
    dummy = RCC_NS->AHB2ENR;                                  \
  }

#define ENABLE_GPIO_PORT_A() ENABLE_GPIO_PORT_BASIC(A)
#define ENABLE_GPIO_PORT_B() ENABLE_GPIO_PORT_BASIC(B)
#define ENABLE_GPIO_PORT_C() ENABLE_GPIO_PORT_BASIC(C)
#define ENABLE_GPIO_PORT_D() ENABLE_GPIO_PORT_BASIC(D)
#define ENABLE_GPIO_PORT_E() ENABLE_GPIO_PORT_BASIC(E)
#define ENABLE_GPIO_PORT_F() ENABLE_GPIO_PORT_BASIC(F)
#define ENABLE_GPIO_PORT_G()         \
  RCC_NS->APB1ENR1 |= RCC_APB1ENR1_PWREN; \
  PWR_NS->CR2      |= PWR_CR2_IOSV;       \
  ENABLE_GPIO_PORT_BASIC(G);
#define ENABLE_GPIO_PORT_H() ENABLE_GPIO_PORT_BASIC(H)

#define ENABLE_GPIO_PORT(port) ENABLE_GPIO_PORT_##port()

#define INPUT              0U
#define OUTPUT             1U
#define ALTERNATE_FUNCTION 2U
#define ANALOG             3U

#define SET_GPIO_MODE(port, pin, mode)                    \
  GPIO##port##_NS->MODER &= ~(GPIO_MODER_MODE##pin##_Msk); \
  GPIO##port##_NS->MODER |= (mode << GPIO_MODER_MODE##pin##_Pos);

#define NO_PUPD   0
#define PULL_UP   1
#define PULL_DOWN 2

#define SET_GPIO_PUPDR(port, pin, mode)                        \
  GPIO##port##_NS->PUPDR &= ~(3U << GPIO_PUPDR_PUPD##pin##_Pos); \
  GPIO##port##_NS->PUPDR |= (mode << GPIO_PUPDR_PUPD##pin##_Pos);

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
  GPIO##port##_NS->AFR[0] &= ~(GPIO_AFRL_AFSEL##pin); \
  GPIO##port##_NS->AFR[0] |= (function << GPIO_AFRL_AFSEL##pin##_Pos);

#define SET_ALTERNATE_FUNCTION_H(port, pin, function) \
  GPIO##port##_NS->AFR[1] &= ~(GPIO_AFRH_AFSEL##pin); \
  GPIO##port##_NS->AFR[1] |= (function << GPIO_AFRH_AFSEL##pin##_Pos);

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

#define MAKE_GPIO_NONSECURE(port, pin) GPIO##port##_NS->SECCFGR &= ~(1 << pin)
#define MAKE_GPIO_SECURE(port, pin)    GPIO##port##_NS->SECCFGR &= ~(0 << pin)

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

#define EXTICR_PORT_A 0x00
#define EXTICR_PORT_B 0x01
#define EXTICR_PORT_C 0x02
#define EXTICR_PORT_D 0x03
#define EXTICR_PORT_E 0x04
#define EXTICR_PORT_F 0x05
#define EXTICR_PORT_G 0x06
#define EXTICR_PORT_H 0x07

#define CONFIGURE_BUTTON_CR(cr, port, pin)                              \
  SET_GPIO_MODE(port, pin, INPUT); \
  SET_GPIO_PUPDR(port, pin, PULL_UP); \
  EXTI_NS->EXTICR[cr - 1] &= ~(EXTI_EXTICR##cr##_EXTI##pin##_Msk);       \
  EXTI_NS->EXTICR[cr - 1] |= (0x0 << EXTI_EXTICR##cr##_EXTI##pin##_Pos); \
  EXTI_NS->IMR1           |= (1 << EXTI_IMR1_IM##pin##_Pos);             \
  EXTI_NS->FTSR1          |= (1 << EXTI_FTSR1_FT##pin##_Pos);            \
  NVIC_SetPriority(EXTI##pin##_IRQn, 2);                                \
  NVIC_EnableIRQ(EXTI##pin##_IRQn);

#define CONFIGURE_BUTTON_B1()                              \
  SET_GPIO_MODE(C, 13, INPUT); \
  SET_GPIO_PUPDR(C, 13, PULL_DOWN); \
  EXTI_NS->EXTICR[3] &= ~(EXTI_EXTICR4_EXTI13_Msk);       \
  EXTI_NS->EXTICR[3] |= (EXTICR_PORT_C << EXTI_EXTICR4_EXTI13_Pos); \
  EXTI_NS->IMR1           |= (1 << EXTI_IMR1_IM13_Pos);             \
  EXTI_NS->RTSR1          |= (1 << EXTI_RTSR1_RT13_Pos);            \
  NVIC_SetPriority(EXTI13_IRQn, 2);                                \
  NVIC_EnableIRQ(EXTI13_IRQn);

typedef void (*button_callback_t)(void);
void add_b1_callback(button_callback_t b1_user_callback);

#endif /* BUTTON_H */