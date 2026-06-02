# Breadboard Buttons

Silly keypad-ish example for wiring several breadboard buttons into GPIO/EXTI.

## Purpose

The shared code creates GPIO and EXTI devices for buttons on several Arduino-style pins. The non-secure side registers callbacks for those buttons.

## TrustZone Split

The secure side configures the clock, UART, board LEDs, and shared button devices. It keeps UART secure and exposes `secure_print` as an NSC function. The non-secure side owns the button callbacks and calls back into secure world to print which pin fired.

This example is mostly about showing custom GPIO/EXTI device creation outside the fixed board LEDs/buttons.
