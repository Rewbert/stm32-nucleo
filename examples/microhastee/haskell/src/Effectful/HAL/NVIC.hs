module Effectful.HAL.NVIC (
    nvic_set_priority,
    nvic_enable_irq,
    nvic_set_target_nonsecure,
) where

import Effectful.Internal.Setup

import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock

import qualified HAL as HAL

nvic_set_priority :: Member Unlocked s => Int -> Int -> Setup ns s ns s ()
nvic_set_priority irqn priority = liftSetupIO $ HAL.nvic_set_priority irqn priority

nvic_enable_irq :: Member Locked s => Int -> Setup ns s ns s ()
nvic_enable_irq irqn = liftSetupIO $ HAL.nvic_enable_irq irqn

nvic_set_target_nonsecure :: Member Unlocked s => Int -> Setup ns s ns s ()
nvic_set_target_nonsecure irqn = liftSetupIO $ HAL.nvic_set_target_nonsecure irqn
