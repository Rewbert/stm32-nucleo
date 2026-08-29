{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
module Effectful.HAL.SysTick (
    systick_configure,
    systick_get_ticks,
    CanDelay(..),
    CanGetTicks(..)
) where

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

systick_configure :: Member Unlocked s => Int -> Setup ns s ns s ()
systick_configure i = liftSetupIO $ HAL.systick_configure i

systick_get_ticks :: Member Unlocked s => Setup ns s ns s Int
systick_get_ticks = liftSetupIO $ HAL.systick_get_ticks

class CanDelay m where
    systick_delay_ms :: Int -> m effects ()

instance CanDelay Secure where
    systick_delay_ms i = secureLiftIO $ HAL.systick_delay_ms i

instance CanDelay Nonsecure where
    systick_delay_ms i = nonsecureLiftIO $ HAL.systick_delay_ms i

-- | Reads the tick counter from within a running Secure/Nonsecure computation
-- (unlike 'systick_get_ticks' above, which only runs during Setup). Named
-- distinctly to avoid clashing with that Setup-phase binding in this module.
class CanGetTicks m where
    systick_ticks :: m effects Int

instance CanGetTicks Secure where
    systick_ticks = secureLiftIO HAL.systick_get_ticks

instance CanGetTicks Nonsecure where
    systick_ticks = nonsecureLiftIO HAL.systick_get_ticks