#ifndef CLOCK_H
#define CLOCK_H

/* invoking this function configures the sysclock to run at 110 MHz, by
   altering the PLL, etc */
void configure_clock(void);

#endif /* CLOCK_H */