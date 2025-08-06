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

void main() {

    ENABLE_IRQ();

    while(1) {
        GPIOA_NS->ODR ^= (1 << 9);
        delay_ms(500);
//        myadd();
    }
}

// GPIOA = 0x42020000
// RCC 0x40021000