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
foreign import ccall "config.h          toggle_blue_led"  toggle :: IO ()

#ifdef SECURE
foreign export ccall "app_main" main :: IO ()
#endif

readPrintModify :: Secure (SRef Int) -> Secure ()
readPrintModify sr = do
    r <- sr
    c <- readSRef r
    unsafeLiftIO $ putStrLn ("current secure state: " ++ show c ++ "\r")
    writeSRef r $ c + 1

secureBlink :: Secure (SRef Int) -> Secure ()
secureBlink r = do
    readPrintModify r
    unsafeLiftIO toggle

loop :: Callable (Secure ()) -> IO ()
loop nsc_f = do
    sg nsc_f
    delay 500
    loop nsc_f

app :: Setup ()
app = do
    ref <- initialSRef 0

    f <- callable $ secureBlink ref
    nonSecure $ loop f

main :: IO ()
main = runSetup app