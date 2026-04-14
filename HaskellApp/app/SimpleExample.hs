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

-- | Evaluates in the TrustZone
secureBlink :: Int -> Int -> (Int -> Int -> Int) -> Secure Int
secureBlink m n f = do
    unsafeLiftIO $ putStrLn $ "message from nonsecure: m = " ++ show m ++ " and n = " ++ show n ++ "\r"
    unsafeLiftIO toggle
    return $ f m n

-- | Nonsecure code, executing outside of the TrustZone (loops forever)
loop :: Callable (Int -> Int -> (Int -> Int -> Int) -> Secure Int) -> Int -> Int -> IO ()
loop nsc_f i j = do
    let fullyAppliedF = nsc_f <.> i <.> j <.> (\m n -> if m `mod` 2 == 0 then m + n else m * n)
    r <- sg fullyAppliedF -- dispatch secure function from nonsecure world

    putStrLn $ "result from secure: " ++ show r ++ "\r"

    delay 500
    loop nsc_f (i + 1) (j + 10)

app :: Setup ()
app = do
    f <- callable $ secureBlink -- mark secureBlink as callable from the nonsecure world
    nonSecure $ loop f 0 0

main :: IO ()
main = runSetup app