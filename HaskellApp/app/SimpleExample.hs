{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE CPP #-}
module SimpleExample where

import HAL

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Control.Monad.IO.Class

-- Conditional compilation to include one of two implementations of the same API
#ifdef SECURE
import Effectful.Secure
#else
import Effectful.NonSecure
#endif

#ifdef SECURE
foreign export ccall "app_main" main :: IO ()
#endif

console_write :: String -> IO ()
console_write str = do
    uart <- board_console
    uart_write uart str

-- | Evaluates in the TrustZone
secureBlink :: GPIO -> Int -> Int -> Secure () Int
secureBlink led m n = do
    secureLiftIO $ console_write $ "message from nonsecure: m = " ++ show m ++ " and n = " ++ show n ++ "\r\n"
    secureLiftIO $ gpio_toggle led
    return $ m + n

-- | Nonsecure code, executing outside of the TrustZone (loops forever)
loop :: Callable (Int -> Int -> Secure () Int) -> Int -> Int -> Nonsecure () ()
loop nsc_f i j = do
    let fullyAppliedF = nsc_f <.> i <.> j
    r <- sg fullyAppliedF -- dispatch secure function from nonsecure world

    nonsecureLiftIO $ console_write $ "result from secure: " ++ show r ++ "\r\n"

    nonsecureLiftIO $ systick_delay_ms 500
    loop nsc_f (i + 1) (j + 10)

board_setup :: IO ()
board_setup = do
    -- initialise the board
    board_init
    board_configure_pll

    -- configure systick handler
    hz <- board_sysclk_hz
    systick_configure $ hz `div` 1000

    -- release the uart to the nonsecure world
    tzsc <- board_tzsc
    periph <- board_console_periph
    tzsc_set_periph tzsc periph TZSC_NONSECURE

    -- initialise and configure the uart
    uart <- board_console
    uart_init uart $ UARTConfig { baudrate = 115200, word_length = 8, stop_bits = 1, parity = NONE }

    -- enable GPIO peripherals
    rcc <- board_rcc
    rcc_enable rcc RCC_GPIOA
    rcc_enable rcc RCC_GPIOB
    rcc_enable rcc RCC_GPIOC

    -- configure and initialise two secure LEDs
    let secure_led_config = GPIOConfig { mode = OUTPUT, pull = NOPULL, alternate = AF0, security_domain = GPIOSecure }
    red_led <- board_led RED
    blue_led <- board_led BLUE
    gpio_init red_led secure_led_config
    gpio_init blue_led secure_led_config

    -- configure and initialise one nonsecure LED
    green_led <- board_led GREEN
    gpio_init green_led $ secure_led_config { security_domain = GPIONonsecure }

    -- finally, enable IRQ
    irq_enable

app = Ix.do
    liftSetupIO $ board_setup

    blue_led <- liftSetupIO $ board_led BLUE

    f <- callable $ secureBlink blue_led -- mark secureBlink as callable from the nonsecure world
    nonsecure $ loop f 0 0

main :: IO ()
main = runSetup app