{-# LANGUAGE QualifiedDo #-}
module RejectDuplicateExti where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.TypeLevel.Lock
import Effectful.HAL

-- REJECTED: 'get_exti' requires the (pin, port) EXTI line to be 'Fresh',
-- exactly like 'get_gpio'. Fetching the same EXTI line twice must fail.
app :: Setup Nil (Cons Unlocked Nil) Nil (Cons (EXTI N0 D) (Cons (EXTI N0 D) (Cons Unlocked Nil))) ()
app = Ix.do
    e1 <- get_exti @N0 @D
    e2 <- get_exti @N0 @D
    Ix.return ()

main :: IO ()
main = runSetup app
