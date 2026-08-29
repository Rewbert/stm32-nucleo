module Effectful.HAL.IRQ (
    irq_enable,
    irq_disable
) where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Internal.Setup

import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock

import qualified HAL as HAL

irq_enable :: Member Locked s => Setup ns s ns s ()
irq_enable = liftSetupIO $ HAL.irq_enable

irq_disable :: Member Unlocked s => Setup ns s ns s ()
irq_disable = liftSetupIO $ HAL.irq_disable