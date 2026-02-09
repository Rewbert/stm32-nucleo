// #include "config.h"

// #include "../MicroHs/src/runtime/eval.c"

#include "hal/gpio.h"
#include "hal/clock.h"
#include "hal/uart.h"
#include "hal/irq.h"
#include "services/led.h"
#include "services/uart.h"

#include "hal/platform/clock.h"

int mhs_main(int argc, char **argv);

void stm32_init() {
    enable_irq();
}

void stm32_exit() {
    toggle_gpio(green_led);
}

void main(void) {
    volatile int x = ticks;

    stm32_init();

    mhs_main(0, 0);

}