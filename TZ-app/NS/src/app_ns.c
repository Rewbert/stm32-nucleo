#include "stm32l5xx.h"

#include "config.h"

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

void exti5_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF5) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF5;
     TOGGLE_LED(A, 9);
//     GPIOA_NS->ODR ^= (1 << 9);
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