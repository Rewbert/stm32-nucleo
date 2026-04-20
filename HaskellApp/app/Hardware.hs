-- | Single-import module that brings all driver and board FFI bindings into scope.
-- Importing this module causes MicroHaskell to typecheck the full FFI surface.
module Hardware where

import HAL.Drivers.SysTick
import HAL.Drivers.GPIO
import HAL.Drivers.UART
import HAL.Drivers.RCC
import HAL.Drivers.PWR
import HAL.Drivers.FLASH
import HAL.Drivers.EXTI
import HAL.Drivers.TZSC
import HAL.Board.Board
import HAL.Board.LED
import HAL.Board.Button
