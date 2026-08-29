{-# LANGUAGE QualifiedDo #-}
module Effectful.HAL.RCC (
    RCC,
    get_rcc,
    HAL.RCCPeriph(..),
    rcc_enable,
    rcc_disable,
    rcc_is_enabled,
    rcc_set_peripheral_clock
) where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Internal.Setup

import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock

import qualified HAL as HAL

data RCC = RCC HAL.RCC

get_rcc :: Member Unlocked s => Setup ns s ns s RCC
get_rcc = Ix.do
    rcc <- liftSetupIO $ HAL.board_rcc
    Ix.return $ RCC rcc

rcc_enable :: Member Unlocked s => RCC -> HAL.RCCPeriph -> Setup ns s ns s ()
rcc_enable (RCC rcc) periph = liftSetupIO $ HAL.rcc_enable rcc periph

rcc_disable :: Member Unlocked s => RCC -> HAL.RCCPeriph -> Setup ns s ns s ()
rcc_disable (RCC rcc) periph = liftSetupIO $ HAL.rcc_disable rcc periph

rcc_is_enabled :: Member Unlocked s => RCC -> HAL.RCCPeriph -> Setup ns s ns s Bool
rcc_is_enabled (RCC rcc) periph = liftSetupIO $ HAL.rcc_is_enabled rcc periph

rcc_set_peripheral_clock :: Member Unlocked s => RCC -> HAL.RCCPeriph -> HAL.RCCPeriphClockSource -> Setup ns s ns s ()
rcc_set_peripheral_clock (RCC rcc) periph source = liftSetupIO $ HAL.rcc_set_peripheral_clock rcc periph source