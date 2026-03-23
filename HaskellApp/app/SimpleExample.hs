{-# LANGUAGE CPP #-}
module SimpleExample where

import Setup

-- Conditional compilation to include one of two implementations of the same API
#ifdef SECURE
import Secure
#else
import NonSecure
#endif

foreign import ccall "drivers/systick.h systick_delay_ms" delay  :: Int -> IO ()
foreign import ccall "config.h   toggle_blue_led" toggle :: IO ()

secureBlink :: Secure ()
secureBlink = unsafeLiftIO toggle

loop :: Callable (Secure ()) -> IO ()
loop nsc_f = do
    sg nsc_f
    delay 500
    loop nsc_f

app :: Setup ()
app = do
    f <- callable secureBlink
    nonSecure $ loop f

main :: IO ()
main = runSetup app