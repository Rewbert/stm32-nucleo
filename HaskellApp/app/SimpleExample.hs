module SimpleExample where

import Setup
--import Secure -- import Secure -- import only one of these, and compile the program twice
import Secure

foreign import ccall "hal/clock.h delay_ms" delay :: Int -> IO ()
foreign import ccall "toggle_green_led" toggle :: IO ()

secureBlink :: Secure ()
secureBlink = do
    unsafeLiftIO toggle
    unsafeLiftIO $ delay 500
    secureBlink

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