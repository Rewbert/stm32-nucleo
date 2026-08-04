-- | Curated, end-user-facing surface for the 'Setup' indexed monad: the opaque type
-- and 'lock_configuration', the one-way configuration/finalization phase gate.
-- 'liftSetupIO' and the rest of "Effectful.Internal.Setup" are for
-- "Effectful.HAL.*" driver-wrapper modules only.
module Effectful.Setup (
    Setup,
    lock_configuration,
) where

import Effectful.Internal.Setup (Setup, lock_configuration)
