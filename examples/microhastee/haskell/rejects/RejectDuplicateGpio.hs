{-# LANGUAGE QualifiedDo #-}
module RejectDuplicateGpio where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.TypeLevel.Lock
import Effectful.HAL

-- REJECTED: 'get_gpio' requires the (pin, port) pair to be 'Fresh' -- not
-- already held anywhere in the capability ledger. Fetching the same pin
-- (PG2) twice in a row must fail to typecheck, even though nothing has been
-- done with the first handle yet.
app :: Setup Nil (Cons Unlocked Nil) Nil (Cons (GPIO N2 G) (Cons (GPIO N2 G) (Cons Unlocked Nil))) ()
app = Ix.do
    p1 <- get_gpio @N2 @G
    p2 <- get_gpio @N2 @G
    Ix.return ()

main :: IO ()
main = runSetup app
