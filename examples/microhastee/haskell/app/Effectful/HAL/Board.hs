{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
module Effectful.HAL.Board (
    board_init,
    board_configure_pll,
    board_sysclk_hz
) where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Internal.Setup

import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock

import qualified HAL as HAL

board_init :: Member Unlocked s => Setup ns s ns s ()
board_init = liftSetupIO $ HAL.board_init

board_configure_pll :: Member Unlocked s => Setup ns s ns s ()
board_configure_pll = liftSetupIO $ HAL.board_configure_pll

board_sysclk_hz :: Member Unlocked s => Setup ns s ns s Int
board_sysclk_hz = liftSetupIO $ HAL.board_sysclk_hz