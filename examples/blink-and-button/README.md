# Blink and Button

Tiny example showing the board abstraction, GPIO, UART, SysTick, EXTI, and one non-secure callable function.

## Purpose

The secure side configures the board, UART, LEDs, and button interrupts. The non-secure side blinks the green LED forever.

## TrustZone Split

The secure program owns setup, UART, EXTI configuration, and the button callbacks. The LED GPIOs are configured as non-secure so the non-secure program can toggle the green LED.
