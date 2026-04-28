{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
module Effectful.HAL.Board (
    board_init,
    board_configure_pll,
    board_sysclk_hz
) where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup

import Effectful.TypeLevel.List

import qualified HAL as HAL

board_init :: Setup ns s ns s ()
board_init = liftSetupIO $ HAL.board_init

board_configure_pll :: Setup ns s ns s ()
board_configure_pll = liftSetupIO $ HAL.board_configure_pll

board_sysclk_hz :: Setup ns s ns s Int
board_sysclk_hz = liftSetupIO $ HAL.board_sysclk_hz