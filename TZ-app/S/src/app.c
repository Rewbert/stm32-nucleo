#include "stm32l5xx.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

#define ENABLE_GPIO_PORT(port)                               \
  {                                                          \
    volatile uint32_t dummy;                                 \
    RCC_S->AHB2ENR |= (1 << RCC_AHB2ENR_GPIO##port##EN_Pos); \
    dummy = RCC_S->AHB2ENR;                                  \
    dummy = RCC_S->AHB2ENR;                                  \
  }

#define INPUT  0x00
#define OUTPUT 0x01

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

void exti5_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF5)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF5;
}

volatile uint32_t ticks;
void systick_handler() {
  ticks++;
}

void delay_ms(uint32_t milliseconds) {
  uint32_t start = ticks;
  uint32_t end = start + milliseconds;

  if (end < start) {
      while (ticks > start);
  }
  while(ticks < end);
}

void secure_app_initialise() {
  SysTick_Config(4000);
  TZ_SysTick_Config_NS(4000);

  CONFIGURE_NONSECURE_BUTTON(A, 5);

  ENABLE_GPIO_PORT(A);
  CONFIGURE_AS_LED(A, 9);
  MAKE_GPIO_NONSECURE(A, 9);

  ENABLE_IRQ();
}

#define NSC __attribute__((cmse_nonsecure_entry))

int NSC add10(int a) {
  return 10+a;
}

// secure GPIOA 0x52020000 <---- my button is here, PA.05
// secure GPIOB 0x52020400
// secure GPIOC 0x52020800
// secure GPIOD 0x52020C00
// secure GPIOE 0x52021000
// secure GPIOF 0x52021400
// secure GPIOG 0x52021800
// secure GPIOH 0x52021C00

// input register for GPIOx is offset 0x10

// secure PWR 0x50007000
// secure SYSCFG 0x5001000
// secure EXTI 0x5002F400, FPR1 is offset by 0x10