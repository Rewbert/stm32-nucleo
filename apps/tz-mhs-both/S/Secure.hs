module Secure where

foreign import ccall "toggle_red_led" toggle :: IO ()

blink :: IO ()
blink = do
    toggle
    putStrLn "Hello Lennart, from TrustZone!\r"

main :: IO ()
main = blink