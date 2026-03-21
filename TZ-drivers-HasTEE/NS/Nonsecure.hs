module Nonsecure where

foreign import ccall "toggle_green_led" toggle :: IO ()

blink :: IO ()
blink = do
    toggle
    putStrLn "Hello Lennart, from nonsecure TrustZone! (MHS instance 2)\r"
    putStrLn "This is a completely separate MHS instance, running in a second, isolated, environment on the same board.\r"
    putStrLn "I cannot observe any memory in the secure enclave!\r"
    putStrLn "My creator, Ser Robert the Large, will experiment with getting the two of us to communicate.\r"
    putStrLn "If he succeeds, we shall all be blessed with confidential computing on microcontrollers using Haskell!\r"
    putStrLn "And I should of course mention, that this is still bare metal programming.\r"

main :: IO ()
main = blink