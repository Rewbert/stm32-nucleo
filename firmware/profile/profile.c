#include "firmware/profile/profile.h"

#include "drivers/rcc.h"

static const uint8_t profile_pins[PROFILE_NUM_PINS] = { 8, 9, 10, 11 }; /* PB8..PB11 */

void profile_init(profile_dev_t *prof, gpio_security_t security) {
    rcc_enable(board_rcc(), RCC_GPIOB);

    gpio_config_t out_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };

    for (int i = 0; i < PROFILE_NUM_PINS; i++) {
        board_gpio_create(&prof->pins[i], BOARD_GPIO_PORT_B, profile_pins[i], &prof->pin_backends[i]);
        gpio_init(&prof->pins[i], &out_cfg);
        gpio_set_security(&prof->pins[i], security);
        gpio_write(&prof->pins[i], GPIO_LOW);
        prof->pin_ptrs[i] = &prof->pins[i];
    }

    board_gpio_port_create(&prof->port, BOARD_GPIO_PORT_B, &prof->port_backend);
}

void profile_emit(profile_dev_t *prof, uint8_t code) {
    gpio_level_t levels[PROFILE_NUM_PINS];

    for (int i = 0; i < PROFILE_NUM_PINS; i++) {
        levels[i] = ((code >> i) & 0x1U) ? GPIO_HIGH : GPIO_LOW;
    }

    gpio_port_write(&prof->port, prof->pin_ptrs, levels, PROFILE_NUM_PINS);
}
