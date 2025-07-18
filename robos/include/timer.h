#ifndef TIMER_H
#define TIMER_H

#include "stm32l5xx.h"

extern uint32_t ticks;

void systick_handler();

// this implements a delay by busy looping
void delay_ms(uint32_t milliseconds);

#endif /* TIMER_H */