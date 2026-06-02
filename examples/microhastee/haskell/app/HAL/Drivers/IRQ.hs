module HAL.Drivers.IRQ (
    irq_enable,
    irq_disable
) where

foreign import ccall "drivers/irq.h irq_enable" c_irq_enable :: IO ()
foreign import ccall "drivers/irq.h irq_disable" c_irq_disable :: IO ()

-- API

irq_enable :: IO ()
irq_enable = c_irq_enable

irq_disable :: IO ()
irq_disable = c_irq_disable