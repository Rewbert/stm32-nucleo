-- | Single-import module that brings all driver and board FFI bindings into scope.
-- Importing this module causes MicroHaskell to typecheck the full FFI surface.
module Hardware where

import Drivers.SysTick
import Drivers.GPIO
import Drivers.UART
import Drivers.RCC
import Drivers.PWR
import Drivers.FLASH
import Drivers.EXTI
import Drivers.TZSC
import Board.Board
import Board.LED
import Board.Button
