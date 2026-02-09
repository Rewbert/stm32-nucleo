void default_handler(void) {
    while(1);
}

void exti_default_handler(void) {
    while(1) {}
  }

void reset_handler(void) __attribute__((weak, alias("default_handler")));
void HardFault_Handler(void) __attribute__((weak, alias("default_handler")));
void secure_fault(void) __attribute__((weak, alias("default_handler")));

void nmi_handler(void) __attribute__((weak, alias("default_handler")));
void mem_handler(void) __attribute__((weak, alias("default_handler")));
void bus_handler(void) __attribute__((weak, alias("default_handler")));
void usage_handler(void) __attribute__((weak, alias("default_handler")));
void sv_handler(void) __attribute__((weak, alias("default_handler")));
void debug_handler(void) __attribute__((weak, alias("default_handler")));
void pend_handler(void) __attribute__((weak, alias("default_handler")));
void systick_handler(void) __attribute__((weak, alias("default_handler")));

void exti0_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti1_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti2_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti3_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti4_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti5_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti6_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti7_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti8_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti9_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti10_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti11_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti12_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti13_handler(void) __attribute__((weak, alias("exti_default_handler")));