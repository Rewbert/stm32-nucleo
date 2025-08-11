#include "stm32l5xx.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

int myadd();

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

#define CONFIGURE_BUTTON_CR(cr, port, pin)                            \
  {                                                                   \
    uint32_t dummy;                                                   \
    RCC_NS->AHB2ENR |= (1 << RCC_AHB2ENR_GPIO##port##EN_Pos);            \
    dummy = RCC_NS->AHB2ENR;                                             \
    dummy = RCC_NS->AHB2ENR;                                             \
  }                                                                   \
  GPIO##port##_NS->MODER &= ~(GPIO_MODER_MODE##pin##_Msk);               \
  GPIO##port##_NS->PUPDR &= ~(GPIO_PUPDR_PUPD##pin##_Msk);               \
  GPIO##port##_NS->PUPDR |= (1 << GPIO_PUPDR_PUPD##pin##_Pos);           \
  EXTI_NS->EXTICR[cr - 1] &= ~(EXTI_EXTICR##cr##_EXTI##pin##_Msk);       \
  EXTI_NS->EXTICR[cr - 1] |= (0x0 << EXTI_EXTICR##cr##_EXTI##pin##_Pos); \
  EXTI_NS->IMR1 |= (1 << EXTI_IMR1_IM##pin##_Pos);                       \
  EXTI_NS->FTSR1 |= (1 << EXTI_FTSR1_FT##pin##_Pos);                     \
  NVIC_SetPriority(EXTI##pin##_IRQn, 2);                        \
  NVIC_EnableIRQ(EXTI##pin##_IRQn);

#define CONFIGURE_NONSECURE_BUTTON(port, pin) CONFIGURE_BUTTON(port, pin)

void exti5_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF5) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF5;
     GPIOA_NS->ODR ^= (1 << 9);
     // user code
  }
}

void main() {
  CONFIGURE_NONSECURE_BUTTON(A, 5);

  ENABLE_IRQ();

  while(1) {
    delay_ms(500);
  }
}

// GPIOA 0x42020000
// RCC   0x40021000
// EXTI  0x4002F400