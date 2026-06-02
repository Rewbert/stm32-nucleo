module Secure where

foreign import ccall "toggle_red_led" toggle :: IO ()

blink :: IO ()
blink = do
    toggle
    putStrLn "Hello Lennart, from TrustZone! (MHS instance 1)\r"
    putStrLn "I am executing in a MHS instance running in the secure enclave of a TrustZone application for the STM32U5 board\r"
    putStrLn "Now, I will hand over execution to the nonsecure world!\r"

main :: IO ()
main = blink
