module RejectLockedOnlyBeforeLock where

import Effectful.Setup
import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock
import Effectful.HAL

-- REJECTED: 'nvic_enable_irq' requires 'Member Locked s' -- it may only run
-- after 'lock_configuration'. Calling it while the ledger still only holds
-- 'Unlocked' must fail to typecheck.
app :: Setup Nil (Cons Unlocked Nil) Nil (Cons Unlocked Nil) ()
app = nvic_enable_irq 0

main :: IO ()
main = runSetup app
