module Nonsecure where

foreign import ccall "toggle_green_led" toggle :: IO ()

blink :: IO ()
blink = do
    toggle
    putStrLn "Hello Lennart, from nonsecure TrustZone!\r"

main :: IO ()
main = blink