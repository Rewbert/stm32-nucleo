module Drivers.SysTick (
      systick_configure
    , systick_get_ticks
    , systick_delay_ms
) where

import Data.Word (Word32)

foreign import ccall "drivers/systick.h systick_configure" configure :: Word32 -> IO ()
foreign import ccall "drivers/systick.h systick_get_ticks" getTicks  :: IO Word32
foreign import ccall "drivers/systick.h systick_delay_ms"  delayMs   :: Word32 -> IO ()

systick_configure :: Int -> IO ()
systick_configure reload = configure (fromInteger $ toInteger reload)

systick_get_ticks :: IO Int
systick_get_ticks = (fromInteger . toInteger) <$> getTicks

systick_delay_ms :: Int -> IO ()
systick_delay_ms ms = delayMs (fromInteger $ toInteger ms)