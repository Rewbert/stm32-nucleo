#include "stm32l5xx.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

int myadd();

void main() {
    while(1) {
        myadd();
    }
}

// GPIOA = 0x42020000
// RCC 0x40021000