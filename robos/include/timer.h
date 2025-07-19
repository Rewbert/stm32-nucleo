#ifndef TIMER_H
#define TIMER_H

#include "stm32l5xx.h"

/* Global ticks variable, to be used in timers */
extern uint32_t ticks;

/* This should be installed as the SysTick handler, as hinted by the name */
void systick_handler();

/* this implements a delay by busy looping. Once I've implemented some threading, there
   should be some more clever implementation that uses interrupts. */
void delay_ms(uint32_t milliseconds);

#endif /* TIMER_H */