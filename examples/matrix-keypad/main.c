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
 * The keypad was soldered with keypad PIN k wired to Dx = D(6-k), i.e.
 * mirrored end-to-end relative to the original plan below. D3 and D4 land on
 * their originally intended role by coincidence (6-3=3, 6-4=2 which is D4's
 * own intended keypad pin); every other line's *role* (row vs col, not just
 * its index) shifts, since D2 (an output in this code) and D5 (an input)
 * trade places. This table reflects the corrected, verified wiring:
 *
 * D0 = PG8  = row 1
 * D1 = PG7  = row 2
 * D2 = PF15 = col 2
 * D3 = PE13 = row 3
 * D4 = PF14 = col 0
 * D5 = PE11 = row 0
 * D6 = PE9  = col 1
 */

#define NUM_ROWS 4
#define NUM_COLS 3

/* Minimum time a raw scan result must hold steady before it's accepted,
 * filtering mechanical/membrane contact bounce. */
#define DEBOUNCE_MS 20

struct button {
    board_gpio_backend_t backend;
    gpio_dev_t gpio;
};

struct button row[NUM_ROWS];
struct button col[NUM_COLS];

/* col[0] (PF14) and col[2] (PF15) share port F, so their reads are batched
 * into a single gpio_port_read() rather than two individual gpio_read()s. */
board_gpio_port_backend_t col_f_backend;
gpio_port_dev_t col_f_port;

static const board_gpio_port_t row_port[NUM_ROWS] = {
    BOARD_GPIO_PORT_E, BOARD_GPIO_PORT_G, BOARD_GPIO_PORT_G, BOARD_GPIO_PORT_E,
};
static const uint8_t row_pin[NUM_ROWS] = { 11, 8, 7, 13 };

static const board_gpio_port_t col_port[NUM_COLS] = {
    BOARD_GPIO_PORT_F, BOARD_GPIO_PORT_E, BOARD_GPIO_PORT_F,
};
static const uint8_t col_pin[NUM_COLS] = { 14, 9, 15 };

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

    board_gpio_port_create(&col_f_port, BOARD_GPIO_PORT_F, &col_f_backend);
}

/**
 * @brief Scan the button matrix to see whether any key is pressed.
 * Assumes only one key is pressed at a time, and does not guarantee
 * anything if more than one key is pressed.
 *
 * @return uint8_t 0 if no key was pressed, and otherwise the ASCII
 */
uint8_t scan_matrix() {
    gpio_dev_t *col_f_devs[2] = { &col[0].gpio, &col[2].gpio };
    gpio_level_t col_f_levels[2];

    for(int i = 0; i < NUM_ROWS; i++) {
        gpio_dev_t *row_gpio = &row[i].gpio;
        gpio_write(row_gpio, GPIO_LOW);

        gpio_level_t col1_level = gpio_read(&col[1].gpio);
        gpio_port_read(&col_f_port, col_f_devs, col_f_levels, 2);

        uint8_t press = 0;
        if(col_f_levels[0] == GPIO_LOW) {
            press = keymap[i][0];
        } else if(col1_level == GPIO_LOW) {
            press = keymap[i][1];
        } else if(col_f_levels[1] == GPIO_LOW) {
            press = keymap[i][2];
        }

        gpio_write(row_gpio, GPIO_HIGH);

        if(press) {
            return press;
        }
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

    /* held: last accepted (debounced) state, reported to UART on change.
     * pending/pending_since: raw scan result currently being watched for
     * DEBOUNCE_MS of stability before it replaces held. */
    uint8_t held = 0;
    uint8_t pending = 0;
    uint32_t pending_since = systick_get_ticks();

    while (1) {
        uint8_t raw = scan_matrix();

        if(raw != pending) {
            pending = raw;
            pending_since = systick_get_ticks();
        } else if(pending != held && (systick_get_ticks() - pending_since) >= DEBOUNCE_MS) {
            held = pending;
            if(held) {
                uart_write(board_console(), &held, 1);
            }
        }
    }
}
