-- | Single-import module that brings all driver and board FFI bindings into scope.
-- Importing this module causes MicroHaskell to typecheck the full FFI surface.
module HAL (
    module HAL.Drivers.SysTick,
    module HAL.Drivers.GPIO,
    module HAL.Drivers.UART,
    module HAL.Drivers.RCC,
    module HAL.Drivers.PWR,
    module HAL.Drivers.FLASH,
    module HAL.Drivers.EXTI,
    module HAL.Drivers.TZSC,
    module HAL.Drivers.IRQ,
    module HAL.Board.Board,
    module HAL.Board.LED,
    module HAL.Board.Button
) where

import HAL.Drivers.SysTick
import HAL.Drivers.GPIO
import HAL.Drivers.UART
import HAL.Drivers.RCC
import HAL.Drivers.PWR
import HAL.Drivers.FLASH
import HAL.Drivers.EXTI
import HAL.Drivers.TZSC
import HAL.Drivers.IRQ
import HAL.Board.Board
import HAL.Board.LED
import HAL.Board.Button
