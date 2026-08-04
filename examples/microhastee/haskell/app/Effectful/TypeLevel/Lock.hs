-- | Phantom markers for the configuration-lock ledger entry. 'Unlocked' is present
-- in the secure-world capability list from the start of every 'Setup' computation
-- (see 'Effectful.Internal.Secure.runSetup' / 'Effectful.Internal.NonSecure.runSetup');
-- 'lock_configuration' is the only function that ever removes it, replacing it with
-- 'Locked'. Nothing ever re-introduces 'Unlocked', so the transition is one-way.
module Effectful.TypeLevel.Lock (
    Locked,
    Unlocked
) where

data Locked
data Unlocked
