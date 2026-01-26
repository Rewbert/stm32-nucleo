module Blinky where

foreign import ccall "toggle_red_led" toggle :: IO ()
foreign import ccall "timer.h delay_ms" c_delay :: Int -> IO ()

blink :: IO ()
blink = do
    toggle
    putStrLn "Hello Lennart, from MHS on STM32L552, bare metal!\r"
    c_delay 500
    blink

main :: IO ()
main = blink