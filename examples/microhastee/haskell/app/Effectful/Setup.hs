-- | Curated, end-user-facing surface for the 'Setup' indexed monad: just the opaque
-- type. 'liftSetupIO' and the rest of "Effectful.Internal.Setup" are for
-- "Effectful.HAL.*" driver-wrapper modules only.
module Effectful.Setup (
    Setup,
) where

import Effectful.Internal.Setup (Setup)
