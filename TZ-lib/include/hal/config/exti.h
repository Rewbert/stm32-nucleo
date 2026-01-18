#ifndef CONFIG_EXTI_H
#define CONFIG_EXTI_H

void exti_make_secure(exti_line_t exti);
void exti_trigger_on(exti_line_t exti, exti_edge_t edge);
void exti_unmask_interrupts(exti_line_t exti);
void exti_route_pin(exti_line_t exti);
void exti_configure_NVIC(exti_line_t, int priority);
void exti_NVIC_set_target_state(exti_line_t exti);

#endif // CONFIG_EXTI_H