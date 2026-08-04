{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
module Effectful.HAL.TZSC (
    TZSC,
    Periph,
    get_tzsc,
    tzsc_release_periph,
    tzsc_share_periph,
    tzsc_lock
) where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Internal.Setup

#ifdef SECURE
import Effectful.Internal.Secure
#else
import Effectful.Internal.NonSecure
#endif

import Effectful.HAL.UART

import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock
import qualified HAL as HAL

data TZSC = TZSC HAL.TZSC

get_tzsc :: Member Unlocked s => Setup ns s ns s TZSC
get_tzsc = Ix.do
    tzsc <- liftSetupIO (HAL.board_tzsc)
    Ix.return $ TZSC tzsc

class Periph periph where
    toTZSCPeriph :: periph -> IO HAL.TZSCPeriph

-- import and add more instances here as needed
instance Periph UART where
    toTZSCPeriph _ = HAL.board_console_periph

tzsc_release_periph :: forall s' periph ns s .
                       ( Member Unlocked s
                       , Member periph s
                       , Delete periph s s'
                       , Periph periph) => TZSC -> periph -> Setup ns s (Cons periph ns) s' ()
tzsc_release_periph (TZSC tzsc) p = Ix.do
    tp <- liftSetupIO $ toTZSCPeriph p
    liftSetupIO $ HAL.tzsc_set_periph tzsc tp HAL.TZSC_NONSECURE

tzsc_share_periph :: ( Member Unlocked s
                     , Member periph s
                     , Periph periph) => TZSC -> periph -> Setup ns s (Cons periph ns) s ()
tzsc_share_periph (TZSC tzsc) p = Ix.do
    tp <- liftSetupIO $ toTZSCPeriph p
    liftSetupIO $ HAL.tzsc_set_periph tzsc tp HAL.TZSC_NONSECURE

tzsc_lock :: Member Unlocked s => TZSC -> Setup ns s ns s ()
tzsc_lock (TZSC tzsc) = liftSetupIO $ HAL.tzsc_lock tzsc