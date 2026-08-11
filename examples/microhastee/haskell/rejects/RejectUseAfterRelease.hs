{-# LANGUAGE QualifiedDo #-}
module RejectUseAfterRelease where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.TypeLevel.Lock
import Effectful.HAL

-- REJECTED: once a pin is released to the nonsecure world, the secure side's
-- 'Setup' actions no longer hold 'Member' evidence for it. Reconfiguring the
-- *old* handle with 'gpio_init' after release must fail to typecheck.
app :: Setup Nil (Cons Unlocked Nil) (Cons (GPIO N2 G) Nil) (Cons Unlocked Nil) ()
app = Ix.do
    p1 <- get_gpio @N2 @G
    gpio_release p1
    gpio_init p1 (GPIOConfig { mode = OUTPUT, pull = NOPULL, alternate = AF0 })

main :: IO ()
main = runSetup app
