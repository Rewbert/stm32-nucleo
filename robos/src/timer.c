#include "stm32l5xx.h"

uint32_t ticks;

void systick_handler() {
    ticks++;
}

// this implements a delay by busy looping
void delay_ms(uint32_t milliseconds) {
    uint32_t start = ticks;
    uint32_t end = start + milliseconds;

    if (end < start) {
        while (ticks > start);
    }
    while(ticks < end);
}