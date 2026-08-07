#include <stdint.h>
#include <stddef.h>

#include "firmware/boards/board.h"
#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/exti.h"
#include "drivers/rcc.h"
#include "drivers/nvic.h"
#include "drivers/timer.h"
#include "drivers/irq.h"

/* TIM6 ticks at TIMER_TICK_HZ; every delay below is expressed directly in
 * these ticks. TIM6's auto-reload register is 16-bit (max 65536 ticks per
 * timer_start() call), so HPERIOD_START_TICKS must stay under that. */
#define TIMER_TICK_HZ 10000U

/* The square wave's half-period, in TIMER_TICK_HZ ticks
 * the button halves it down to
 * HPERIOD_FLOOR_TICKS (100 ticks = 10 ms, a 50 Hz square wave) before
 * wrapping back to the start. */
#define HPERIOD_START_TICKS 5000U
#define HPERIOD_FLOOR_TICKS 100U

/*
 * The SSM scheduler (vendor/ssm) allocates one activation record per routine
 * call via SSM_ACT_MALLOC/SSM_ACT_FREE, which default to malloc()/free(). Rather
 * than pull in newlib's heap (and the syscall stubs it needs), hand it a plain
 * bump allocator out of a static arena. Unlike a one-shot computation, sigGen
 * below runs forever without ever leaving, so it only ever makes the one
 * ssm_enter() call at startup — there's nothing to reclaim, and not much to
 * allocate either.
 */
#define ACT_POOL_BYTES 1024U

static uint8_t act_pool[ACT_POOL_BYTES];
static size_t  act_pool_used = 0;

static void *act_pool_alloc(size_t size) {
    size = (size + 7U) & ~(size_t)7U; /* keep 8-byte alignment for ssm_time_t fields */
    if (act_pool_used + size > ACT_POOL_BYTES) {
        while (1) { } /* out of memory: shouldn't happen, see ACT_POOL_BYTES above */
    }
    void *p = &act_pool[act_pool_used];
    act_pool_used += size;
    return p;
}

#define SSM_ACT_MALLOC(size)    act_pool_alloc(size)
#define SSM_ACT_FREE(ptr, size) ((void)(ptr))

#include "ssm.h"

void ssm_throw(int reason, const char *file, int line, const char *func) {
    (void)reason;
    (void)file;
    (void)line;
    (void)func;
    while (1) { }
}

void exit(int status) {
    (void)status;
    while (1) { }
}

static volatile uint32_t hperiod_ticks = HPERIOD_START_TICKS;

typedef struct {
    SSM_ACT_FIELDS;
    ssm_event_t timer;
    struct ssm_trigger trigger1;
} siggen_act_t;

ssm_stepf_t step_siggen;

siggen_act_t *enter_siggen(struct ssm_act *parent, ssm_priority_t priority, ssm_depth_t depth) {
    siggen_act_t *act = (siggen_act_t *) ssm_enter(sizeof(siggen_act_t), step_siggen, parent,
                                                    priority, depth);
    ssm_initialize_event(&act->timer);
    act->trigger1.act = (struct ssm_act *) act;
    return act;
}

void step_siggen(struct ssm_act *cont) {
    siggen_act_t *act = (siggen_act_t *) cont;
    switch (act->pc) {
    case 0:
        for (;;) {
            ssm_later_event(&act->timer, ssm_now() + hperiod_ticks);
            ssm_sensitize((struct ssm_sv *) &act->timer, &act->trigger1);
            act->pc = 1;
            return;
        case 1:
            if (ssm_event_on((struct ssm_sv *) &act->timer)) {
                ssm_desensitize(&act->trigger1);
                gpio_toggle(board_led(BOARD_LED_GREEN));
            } else {
                return;
            }
        }
    }
}

/* ---- remoteControl (button_pressed), and reporting the current period ---- */

static uint8_t append_str(char *out, const char *s) {
    uint8_t len = 0;
    while (*s) {
        out[len++] = *s++;
    }
    return len;
}

static uint8_t append_udec(char *out, uint32_t v) {
    char tmp[10];
    uint8_t n = 0;
    if (v == 0) {
        out[0] = '0';
        return 1;
    }
    while (v > 0) {
        tmp[n++] = (char)('0' + (v % 10U));
        v /= 10U;
    }
    uint8_t len = n;
    while (n > 0) {
        *out++ = tmp[--n];
    }
    return len;
}

static void report_hperiod(uint32_t ticks) {
    char buf[48];
    uint8_t len = 0;
    len += append_str(buf + len, "hperiod = ");
    len += append_udec(buf + len, ticks);
    len += append_str(buf + len, " ticks (~");
    len += append_udec(buf + len, TIMER_TICK_HZ / (2U * ticks));
    len += append_str(buf + len, " Hz)\r\n");
    uart_write(board_console(), (const uint8_t *)buf, len);
}

static void button_pressed(exti_edge_t edge) {
    (void)edge;
    uint32_t h = hperiod_ticks;
    h = (h <= HPERIOD_FLOOR_TICKS) ? HPERIOD_START_TICKS : (h / 2U);
    hperiod_ticks = h;
    report_hperiod(h);
}

/* TIM6 callback: run the scheduler up to the current instant, then arm TIM6
 * for the gap until the next one. sigGen loops forever, always scheduling
 * its next toggle before this returns, so there's always a next instant. */
static void advance(void) {
    ssm_tick();
    timer_start(board_timer(), (uint32_t)(ssm_next_event_time() - ssm_now()));
}

void main(void) {
    board_init();
    board_configure_pll();

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);

    rcc_enable(board_rcc(), RCC_GPIOC); /* green LED (PC7) and user button (PC13) */
    rcc_enable(board_rcc(), RCC_TIM6);

    gpio_config_t led_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_led(BOARD_LED_GREEN), &led_cfg);

    gpio_config_t button_cfg = {
        .mode      = GPIO_MODE_INPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_button(BOARD_BUTTON_USER), &button_cfg);

    exti_config_t button_exti_cfg = {
        .port = EXTI_PORT_C,
        .pin  = 13, /* the user button is PC13, active low */
        .edge = EXTI_EDGE_FALLING,
    };
    exti_init(board_button_exti(BOARD_BUTTON_USER), &button_exti_cfg);

    int timer_irq = timer_irqn(board_timer());
    nvic_set_priority(timer_irq, 0);
    nvic_enable_irq(timer_irq);

    int button_irq = exti_irqn(board_button_exti(BOARD_BUTTON_USER));
    nvic_set_priority(button_irq, 0); /* same priority as TIM6: never preempts it, so
                                          hperiod_ticks never needs locking */
    nvic_enable_irq(button_irq);

    ssm_activate((struct ssm_act *) enter_siggen(&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH));

    timer_config_t timer_cfg = {
        .prescaler = (uint16_t)((board_sysclk_hz() / TIMER_TICK_HZ) - 1U),
    };
    timer_init(board_timer(), &timer_cfg);
    timer_register_callback(board_timer(), advance);

    exti_register_callback(board_button_exti(BOARD_BUTTON_USER), button_pressed);

    irq_enable();

    report_hperiod(hperiod_ticks);
    advance(); /* runs instant 0 (schedules sigGen's first toggle) and arms TIM6 for it */

    while (1) {
    }
}
