#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32u5/timer.h"

#include "stm32u5xx.h"

/* TIM6 register layout, RCC enable bit, and IRQ number (49) are identical to STM32L5.
 * Only one TIM6 is wired up on this board, so a single static callback slot
 * (rather than a per-instance array like EXTI) is enough. */
static timer_callback_t tim6_callback = 0;

static void stm32u5_timer_init(struct timer_dev *dev, timer_config_t *config) {
    stm32u5_timer_backend_t *backend = (stm32u5_timer_backend_t *) dev->backend;

    backend->tim->CR1  &= ~TIM_CR1_CEN;
    backend->tim->PSC   = config->prescaler;
    backend->tim->CR1  |= TIM_CR1_OPM;  // one-pulse mode: CEN clears itself on the next update event
    backend->tim->EGR   = TIM_EGR_UG;   // latch PSC now, reset CNT to 0
    backend->tim->SR    = 0;            // UG above also raises UIF; clear it so init doesn't leave a stale flag
}

static void stm32u5_timer_register_callback(struct timer_dev *dev, timer_callback_t cb) {
    (void)dev;
    tim6_callback = cb;
}

static void stm32u5_timer_start(struct timer_dev *dev, uint32_t ticks) {
    stm32u5_timer_backend_t *backend = (stm32u5_timer_backend_t *) dev->backend;

    backend->tim->CR1  &= ~TIM_CR1_CEN;
    backend->tim->ARR   = (ticks - 1) & 0xFFFFU; // TIM6 is a 16-bit counter
    backend->tim->EGR   = TIM_EGR_UG;            // force CNT=0 and latch the new ARR/PSC immediately
    backend->tim->SR    = 0;                     // clear the UIF that UG above raises, so we don't fire right away
    backend->tim->DIER |= TIM_DIER_UIE;
    backend->tim->CR1  |= TIM_CR1_CEN;
}

static void stm32u5_timer_stop(struct timer_dev *dev) {
    stm32u5_timer_backend_t *backend = (stm32u5_timer_backend_t *) dev->backend;
    backend->tim->CR1 &= ~TIM_CR1_CEN;
}

static int stm32u5_timer_irqn(struct timer_dev *dev) {
    (void)dev;
    return TIM6_IRQn;
}

static const timer_driver_api_t stm32u5_timer_api = {
    .init              = stm32u5_timer_init,
    .register_callback = stm32u5_timer_register_callback,
    .start             = stm32u5_timer_start,
    .stop              = stm32u5_timer_stop,
    .irqn              = stm32u5_timer_irqn,
};

void stm32u5_timer_create(timer_dev_t *dev, TIM_TypeDef *tim, stm32u5_timer_backend_t *backend_storage) {
    dev->api              = &stm32u5_timer_api;
    dev->backend          = backend_storage;
    backend_storage->tim  = tim;
}

void tim6_handler(void) {
    if (TIM6x->SR & TIM_SR_UIF) {
        TIM6x->SR = 0;
        if (tim6_callback) {
            tim6_callback();
        }
    }
}
