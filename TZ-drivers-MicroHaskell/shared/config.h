#ifndef CONFIG_STM32L5_H
#define CONFIG_STM32L5_H

#define PACKED // don't want packed on Cortex M

#define WANT_STDIO 1

#define WANT_FLOAT 1
#define WANT_FLOAT32 1

#define WANT_MD5 0

#define WANT_TICK 0

#define WANT_TICK 0

#define WANT_ARGS 0

#define GCRED    0
#define FASTTAGS 1
#define INTTABLE 1
#define SANITY   1
#define STACKOVL 1

#define HEAP_CELLS 8000
#define STACK_SIZE 500

// #define INITIALIZATION
// void main_setup(void);

void stm32_exit(int n);
#define EXIT stm32_exit

int ffs(int x);
#define FFS ffs

void toggle_red_led(void);
void toggle_green_led(void);

int unlink(void *x);

#define ERR(str) while(1) {}
#define ERR1(sr,a) while(1) {}

#endif /* CONFIG_STM32L5_H */