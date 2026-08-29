-- | Curated, end-user-facing surface for the non-secure-build 'Secure'/'Nonsecure' monads.
-- Must export the exact same names as "Effectful.Secure" -- the same application source
-- compiles against whichever one the @-DSECURE@ toggle selects.
module Effectful.NonSecure (
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

import Effectful.Internal.NonSecure
