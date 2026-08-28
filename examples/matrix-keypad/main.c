#include <stdint.h>
#include <stdio.h>

#include "firmware/boards/board.h"
#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/systick.h"
#include "drivers/irq.h"
#include "drivers/rcc.h"

/* To ease my life, I will use the Arduino pins on my MCU. They are female,
 * and I will require no soldering to use them. Their designation does not
 * reflect the actual pin assignment, however, so we must remember which
 * Arduino name maps to which actual pin, and how we assign them to our
 * button matrix, and which button matrix output pin they refer to.
 *
 * D0 = PG8  = row 0 = PIN 1
 * D1 = PG7  = row 1 = PIN 6
 * D2 = PF15 = row 2 = PIN 5
 * D3 = PE13 = row 3 = PIN 3
 * D4 = PF14 = col 0 = PIN 2
 * D5 = PE11 = col 1 = PIN 0
 * D6 = PE9  = col 2 = PIN 4
 */

#define NUM_ROWS 4
#define NUM_COLS 3

struct button {
    board_gpio_backend_t backend;
    gpio_dev_t gpio;
};

struct button row[NUM_ROWS];
struct button col[NUM_COLS];

static const board_gpio_port_t row_port[NUM_ROWS] = {
    BOARD_GPIO_PORT_G, BOARD_GPIO_PORT_G, BOARD_GPIO_PORT_F, BOARD_GPIO_PORT_E,
};
static const uint8_t row_pin[NUM_ROWS] = { 8, 7, 15, 13 };

static const board_gpio_port_t col_port[NUM_COLS] = {
    BOARD_GPIO_PORT_F, BOARD_GPIO_PORT_E, BOARD_GPIO_PORT_E,
};
static const uint8_t col_pin[NUM_COLS] = { 14, 11, 9 };

/* What my actual keypad looks like */
uint8_t keymap[NUM_ROWS][NUM_COLS] = {
    { '1', '2', '3' },
    { '4', '5', '6' },
    { '7', '8', '9' },
    { '*', '0', '#' },
};

static void configure_gpio(void) {
    rcc_enable(board_rcc(), RCC_GPIOE);
    rcc_enable(board_rcc(), RCC_GPIOF);
    rcc_enable(board_rcc(), RCC_GPIOG);

    gpio_config_t row_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    for (int i = 0; i < NUM_ROWS; i++) {
        board_gpio_create(&row[i].gpio, row_port[i], row_pin[i], &row[i].backend);
        gpio_init(&row[i].gpio, &row_cfg);
        gpio_write(&row[i].gpio, GPIO_HIGH); /* idle high; each row is driven low in turn to scan */
    }

    gpio_config_t col_cfg = {
        .mode = GPIO_MODE_INPUT,
        .pull = GPIO_PULLUP,
        .alternate = GPIO_AF0,
    };
    for(int i = 0; i < NUM_COLS; i++) {
        board_gpio_create(&col[i].gpio, col_port[i], col_pin[i], &col[i].backend);
        gpio_init(&col[i].gpio, &col_cfg);
        // do I default write it to something?
    }
}

/**
 * @brief Scan the button matrix to see whether any key is pressed.
 * Assumes only one key is pressed at a time, and does not guarantee
 * anything if more than one key is pressed.
 *
 * @return uint8_t 0 if no key was pressed, and otherwise the ASCII
 */
uint8_t scan_matrix() {
    for(int i = 0; i < NUM_ROWS; i++) {
        gpio_dev_t *row_gpio = &row[i].gpio;
        gpio_write(row_gpio, GPIO_LOW);

        for(int j = 0; j < NUM_COLS; j++) {
            gpio_dev_t *col_gpio = &col[j].gpio;
            gpio_level_t level = gpio_read(col_gpio);

            if(level == GPIO_LOW) {
                gpio_write(row_gpio, GPIO_HIGH);
                return keymap[i][j];
            }
        }

        gpio_write(row_gpio, GPIO_HIGH);
    }
    return 0;
}

void main(void) {
    board_init();
    board_configure_pll();
    systick_configure(board_sysclk_hz() / 1000);

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);

    configure_gpio();

    irq_enable();

    uart_write(board_console(), "booted\n\r", 8);
    while (1) {
        uint8_t press = scan_matrix();
        if(press) {
            uart_write(board_console(), &press, 1);
        }
    }
}
