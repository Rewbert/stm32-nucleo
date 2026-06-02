{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
module Effectful.HAL.SysTick (
    systick_configure,
    systick_get_ticks,
    CanDelay(..)
) where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup

#ifdef SECURE
import Effectful.Secure
#else
import Effectful.NonSecure
#endif

import Effectful.TypeLevel.List

import qualified HAL as HAL

systick_configure :: Int -> Setup ns s ns s ()
systick_configure i = liftSetupIO $ HAL.systick_configure i

systick_get_ticks :: Setup ns s ns s Int
systick_get_ticks = liftSetupIO $ HAL.systick_get_ticks

class CanDelay m where
    systick_delay_ms :: Int -> m effects ()

instance CanDelay Secure where
    systick_delay_ms i = secureLiftIO $ HAL.systick_delay_ms i

instance CanDelay Nonsecure where
    systick_delay_ms i = nonsecureLiftIO $ HAL.systick_delay_ms i