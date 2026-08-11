{-# LANGUAGE QualifiedDo #-}
module RejectConfigureAfterLock where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.TypeLevel.Lock
import Effectful.HAL

-- REJECTED: 'lock_configuration' removes 'Unlocked' from the ledger and is a
-- one-way transition (see 'Effectful.Internal.Setup.lock_configuration').
-- Every configuration action ('get_gpio' among them) requires 'Member
-- Unlocked s', so calling one after locking must fail to typecheck.
app :: Setup Nil (Cons Unlocked Nil) Nil (Cons (GPIO N2 G) (Cons Locked Nil)) ()
app = Ix.do
    lock_configuration
    p1 <- get_gpio @N2 @G
    Ix.return ()

main :: IO ()
main = runSetup app
