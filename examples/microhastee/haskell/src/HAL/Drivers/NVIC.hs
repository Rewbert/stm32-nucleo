module HAL.Drivers.NVIC (
    nvic_set_priority,
    nvic_enable_irq,
    nvic_set_target_nonsecure,
) where

import Foreign.C.Types

foreign import ccall "drivers/nvic.h nvic_set_priority" c_nvic_set_priority :: CInt -> CUChar -> IO ()
foreign import ccall "drivers/nvic.h nvic_enable_irq" c_nvic_enable_irq :: CInt -> IO ()
foreign import ccall "drivers/nvic.h nvic_set_target_nonsecure" c_nvic_set_target_nonsecure :: CInt -> IO ()

nvic_set_priority :: Int -> Int -> IO ()
nvic_set_priority irqn priority =
    c_nvic_set_priority (fromIntegral irqn) (fromIntegral priority)

nvic_enable_irq :: Int -> IO ()
nvic_enable_irq irqn = c_nvic_enable_irq (fromIntegral irqn)

nvic_set_target_nonsecure :: Int -> IO ()
nvic_set_target_nonsecure irqn = c_nvic_set_target_nonsecure (fromIntegral irqn)
