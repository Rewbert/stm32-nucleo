-- | Curated, end-user-facing surface for the secure-build 'Secure'/'Nonsecure' monads.
-- Deliberately narrower than "Effectful.Internal.Secure": it hides the raw data
-- constructors and the unconstrained IO-lifting primitives (secureLiftIO, nonsecureLiftIO,
-- secureToIO, nonsecureToIO), which are for "Effectful.HAL.*" driver-wrapper modules only.
module Effectful.Secure (
    Secure,
    Nonsecure,
    Callable,
    callable,
    (<.>),
    sg,
    nonsecure,
    runSetup,
    SRef,
    initialSRef,
    readSRef,
    writeSRef,
    modifySRef,
    NSRef,
    initialNSRef,
    readNSRef,
    writeNSRef,
    modifyNSRef,
) where

import Effectful.Internal.Secure
