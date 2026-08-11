{-# LANGUAGE QualifiedDo #-}
module RejectRefetchAfterRelease where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.TypeLevel.Lock
import Effectful.HAL

-- REJECTED: 'gpio_release' moves a pin from the secure capability list to
-- the nonsecure one -- it does not make the pin disappear. 'get_gpio' must
-- still refuse to hand out a second handle to the same physical pin after
-- release: 'Fresh' is checked against *both* lists, not just the one
-- currently being extended.
app :: Setup Nil (Cons Unlocked Nil) (Cons (GPIO N2 G) Nil) (Cons (GPIO N2 G) (Cons Unlocked Nil)) ()
app = Ix.do
    p1 <- get_gpio @N2 @G
    gpio_release p1
    p2 <- get_gpio @N2 @G
    Ix.return ()

main :: IO ()
main = runSetup app
