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

secureBlink :: Secure (SRef Int) -> Int -> Int -> Secure Int
secureBlink r m n = do
    readPrintModify r
    unsafeLiftIO $ putStrLn $ "message from nonsecure: m = " ++ show m ++ " and n = " ++ show n ++ "\r"
    unsafeLiftIO toggle
    return $ m + n

loop :: Callable (Int -> Int -> Secure Int) -> Int -> Int -> IO ()
loop nsc_f i j = do
    r <- sg $ nsc_f <.> i <.> j
    putStrLn $ "result from secure: " ++ show r ++ "\r"
    delay 500
    loop nsc_f (i + 1) (j + 10)

app :: Setup ()
app = do
    ref <- initialSRef 0

    f <- callable $ secureBlink ref
    nonSecure $ loop f 0 0

main :: IO ()
main = runSetup app