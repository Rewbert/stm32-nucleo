{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
-- | Thin 'Setup'/'Secure' wrapper around the raw 'HAL.Drivers.UDB' bindings, following
-- the same shape as "Effectful.HAL.UART"/"Effectful.HAL.GPIO": the driver-level IO is
-- lifted through 'secureLiftIO', which is only reachable from here (an
-- @Effectful.HAL.*@ module), never from application code directly.
--
-- 'udb_insert'/'udb_lookup' are typed to run in 'Secure' only -- there is no
-- 'Nonsecure' instance -- so a domain's persistent store is reachable exclusively
-- from the secure world at the type level, not just by convention.
module Effectful.HAL.UDB (
    UDB,
    udb_init,
    udb_insert,
    udb_lookup,
) where

import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)

import qualified Control.Monad.IxMonad as Ix
import Effectful.Internal.Setup

#ifdef SECURE
import Effectful.Internal.Secure
#else
import Effectful.Internal.NonSecure
#endif

import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock
import qualified HAL as HAL

-- | A domain's own persistent key-value store (see 'HAL.Drivers.UDB' -- each of
-- secure/nonsecure gets an independent database backed by its own flash pages).
data UDB = UDB HAL.UDB

-- | Mount this domain's database, formatting it on first boot. Does not touch the
-- capability ledger -- like 'Effectful.HAL.TZSC.get_tzsc', this just hands back a
-- plain value to close over later.
udb_init :: Member Unlocked s => Setup ns s ns s UDB
udb_init = Ix.do
    db <- liftSetupIO HAL.init_db
    Ix.return $ UDB db

udb_insert :: (Hashable key, NFData val) => UDB -> key -> val -> Secure effects ()
udb_insert (UDB db) key val = secureLiftIO $ HAL.insert key val db

udb_lookup :: Hashable key => UDB -> key -> Secure effects (Maybe a)
udb_lookup (UDB db) key = secureLiftIO $ HAL.lookup key db
