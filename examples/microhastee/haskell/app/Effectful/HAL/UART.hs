{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
module Effectful.HAL.UART (
    UART,
    HAL.UARTParity(..),
    HAL.UARTConfig(..),
    uart_init,
    get_console,
    UARTActions(..)
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

data UART = UART HAL.UART

get_console :: Setup ns s ns (Cons UART s) UART
get_console = Ix.do
    uart <- liftSetupIO $ HAL.board_console
    Ix.return $ UART uart

class UARTActions m where
    uart_write :: (Member UART effects) => UART -> String -> m effects ()
    uart_read :: (Member UART effects) => UART -> m effects String

instance UARTActions Secure where
    uart_write (UART uart) str = secureLiftIO $ HAL.uart_write uart str

    uart_read (UART uart) = secureLiftIO $ HAL.uart_read uart

instance UARTActions Nonsecure where
    uart_write (UART uart) str = nonsecureLiftIO $ HAL.uart_write uart str

    uart_read (UART uart) = nonsecureLiftIO $ HAL.uart_read uart

uart_init :: UART -> HAL.UARTConfig -> Setup ns s ns s ()
uart_init (UART uart) cfg = liftSetupIO $ HAL.uart_init uart cfg