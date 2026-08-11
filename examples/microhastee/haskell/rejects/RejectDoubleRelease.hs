{-# LANGUAGE QualifiedDo #-}
module RejectDoubleRelease where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.TypeLevel.Lock
import Effectful.HAL

-- REJECTED: 'gpio_release' requires 'Member (GPIO pin port) s'. Once
-- released, the pin is gone from the secure list, so releasing it a second
-- time must fail to typecheck.
app :: Setup Nil (Cons Unlocked Nil) (Cons (GPIO N2 G) (Cons (GPIO N2 G) Nil)) (Cons Unlocked Nil) ()
app = Ix.do
    p1 <- get_gpio @N2 @G
    gpio_release p1
    gpio_release p1

main :: IO ()
main = runSetup app
