
#include "hal/irq.h"
#include "hal/gpio.h"
#include "hal/exti.h"
#include "hal/uart.h"
#include "hal/platform/flash.h"
#include "hal/platform/clock.h"
#include "hal/platform/domain.h"

#include "services/button.h"
#include "services/led.h"
#include "services/uart.h"

#include "shared.h"

/***** These are the NONSECURE_CALLABLE functions declared by the secure application */

extern int changePin(uint32_t *old, uint32_t *new);
extern int unlockDoor(uint32_t *code);
extern int lockDoor(uint32_t *code);

/*************************************************************************************/

/**
 * @brief The last key that was pressed
 * 
 */
int key = 0;

/**
 * @brief A global flag indicating whether there is a new keypress event
 * 
 */
int keyready = 0;

/**
 * @brief Different kind of keys that can be pressed. I have these hard-wired to a breadboard
 * for my little demo.
 * 
 */
typedef enum {
    ONE=1,
    TWO,
    THREE,
    FOUR,
    CHANGE_PIN,
    LOCK,
    UNLOCK
} key_t ;

/***** These are simple button callbacks *****/

#define DEFINE_BUTTON_CB(name, value) \
void name(exti_edge_t edge) {         \
    (void)edge;                       \
    key = value;                      \
    keyready = 1;                     \
}

DEFINE_BUTTON_CB(button_a2_callback, CHANGE_PIN)
DEFINE_BUTTON_CB(button_a3_callback, LOCK)
DEFINE_BUTTON_CB(button_a5_callback, ONE)
DEFINE_BUTTON_CB(button_a6_callback, TWO)
DEFINE_BUTTON_CB(button_a7_callback, THREE)
DEFINE_BUTTON_CB(button_a8_callback, FOUR)
DEFINE_BUTTON_CB(button_a10_callback, UNLOCK)

/***** End of the simple button callbacks *****/

/**
 * @brief Read and return a key if one is pressed within the timeout limit.
 * 
 * @return uint32_t The key that was pressed, or -1 if the timeout occurred.
 */
uint32_t read_key(uint32_t timeout) {
    uint32_t current = get_ticks();
    uint32_t end = current + timeout;

    // wait until a key has been pressed, or the timeout has expired
    while(!keyready) {
        if(get_ticks() > end) return -1;
    }

    keyready = 0;
    return (uint32_t) key;
}

/**
 * @brief Read a sequence of 4 key presses, constituting a pin code.
 * 
 * @param pin A pointer to which the 4 key-presses will be written. Will only be written to if
 * all four keys were pressed.
 */
int readPin(uint32_t *pin) {
    uart_write_string(HAL_LPUART1, "> ");

    uint32_t read[4];
    for(int i = 0; i < 4; i++) {
        int key = read_key(10000); // 10 seconds
        if(key == -1) {
            return 0;
        }
        uart_write(HAL_LPUART1, 0x30 + key);
        read[i] = key;
    }

    uart_write_string(HAL_LPUART1, "\r\n");

    for(int i = 0; i < 4; i++) {
        pin[i] = read[i];
    }

    return 1;
}

/**
 * @brief Change the pin code. This is done by reading the old pin, then the new pin, and then
 * asking the secure application to change the pin. If the old pin was correct, the pin code will
 * be changed.
 * 
 */
void change_pin() {
    uint32_t pin[4];
    readPin(pin);

    uint32_t new[4];
    readPin(new);

    if(changePin(pin, new)) {
        uart_write_string(HAL_LPUART1, "Pin changed\r\n");
    } else {
        uart_write_string(HAL_LPUART1, "Wrong pin entered\r\n");
    }
}

/**
 * @brief Unlock the door. This is done by reading a pin code and asking the secure application to
 * unlock it. The secure application will only do so if it can properly authenticale the old pin.
 * 
 */
void unlock_door() {
    uint32_t pin[4];
    readPin(pin);

    if(unlockDoor(pin)) {
        uart_write_string(HAL_LPUART1, "Door unlocked\r\n");
    } else {
        uart_write_string(HAL_LPUART1, "Wrong pin entered\r\n");
    }
}

/**
 * @brief Lock the door. This is done by reading a pin code and asking the secure application to
 * lock it. The secure application will only do so if it can properly authenticale the old pin.
 * 
 */
void lock_door() {
    uint32_t pin[4];
    readPin(pin);

    if(lockDoor(pin)) {
        uart_write_string(HAL_LPUART1, "Door locked\r\n");
    } else{
        uart_write_string(HAL_LPUART1, "Wrong pin entered\r\n");
    }
}

/**
 * @brief The application is a control panel that is constantly ready to serve requests. A button is
 * pressed to indicate which action is requestion, after which one or two pin codes need to be entered.
 * While the control panel would also have a display, the display is mocked with UART today, since I
 * have no display to use.
 * 
 */
void app() {
    while(1) {
        uart_write_string(HAL_LPUART1, "Please select a command\r\n");
        uint32_t command = read_key(0x0FFFFFFF);

        switch(command) {
            case CHANGE_PIN: uart_write_string(HAL_LPUART1, "To change the code, please enter the current code followed by the new code\r\n"); change_pin();  break;
            case LOCK:       uart_write_string(HAL_LPUART1, "To lock the door, please enter the code\r\n");                                    lock_door();   break;
            case UNLOCK:     uart_write_string(HAL_LPUART1, "To unlock the door, please enter the code\r\n");                                  unlock_door(); break;
            default: continue; 
        }
    }
}

void main(void) {
    register_button_callback(a2, &button_a2_callback);
    register_button_callback(a3, &button_a3_callback);
    register_button_callback(a5, &button_a5_callback);
    register_button_callback(a6, &button_a6_callback);
    register_button_callback(a7, &button_a7_callback);
    register_button_callback(a8, &button_a8_callback);
    register_button_callback(a10, &button_a10_callback);

    enable_irq();

    app();
}