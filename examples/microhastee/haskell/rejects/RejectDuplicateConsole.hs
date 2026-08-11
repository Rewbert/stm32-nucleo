{-# LANGUAGE QualifiedDo #-}
module RejectDuplicateConsole where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock
import Effectful.HAL

-- REJECTED: 'get_console' requires 'UART' to be 'Fresh'. There is only one
-- physical console UART -- fetching a second handle to it must fail to
-- typecheck.
app :: Setup Nil (Cons Unlocked Nil) Nil (Cons UART (Cons UART (Cons Unlocked Nil))) ()
app = Ix.do
    u1 <- get_console
    u2 <- get_console
    Ix.return ()

main :: IO ()
main = runSetup app
