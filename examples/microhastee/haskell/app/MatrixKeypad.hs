{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE CPP #-}
module MatrixKeypad where

import qualified Control.Monad.IxMonad as Ix
import Control.Monad.IO.Class
import MicroHasTEE

import MatrixKeypad.Secure
import MatrixKeypad.NonSecure

#ifdef SECURE
foreign export ccall "app_main" main :: IO ()
#endif

-- * Setup ---------------------------------------------------------------------

app :: Setup Nil InitialSecure NonsecureEffects SecureEffects ()
app = Ix.do
    -- init board and configure frequency
    board_init
    board_configure_pll

    -- set systick handler to fire every ms
    hz <- board_sysclk_hz
    systick_configure (hz `div` 1000)

    -- configure UART and release it to the nonsecure domain -- shared by both worlds
    uart <- get_console
    uart_init uart $ UARTConfig { baudrate = 115200, word_length = 8, stop_bits = 1, parity = NONE }
    tzsc <- get_tzsc
    tzsc_release_periph tzsc uart

    -- enable GPIO ports: C for the gate/lockout LED, E/F/G for the keypad
    rcc <- get_rcc
    rcc_enable rcc RCC_GPIOC
    rcc_enable rcc RCC_GPIOE
    rcc_enable rcc RCC_GPIOF
    rcc_enable rcc RCC_GPIOG

    let outCfg = GPIOConfig { mode = OUTPUT, pull = NOPULL,  alternate = AF0 }
        colCfg = GPIOConfig { mode = INPUT,  pull = PULLUP,  alternate = AF0 }

    -- secure-only: solenoid gate and lockout LED, never released
    gate       <- get_gpio @N3 @C
    lockoutLed <- get_gpio @N2 @G
    gpio_init gate outCfg
    gpio_init lockoutLed outCfg

    row0 <- get_gpio @N11 @E
    row1 <- get_gpio @N8  @G
    row2 <- get_gpio @N7  @G
    row3 <- get_gpio @N13 @E
    gpio_init row0 outCfg
    gpio_init row1 outCfg
    gpio_init row2 outCfg
    gpio_init row3 outCfg

    col0 <- get_gpio @N14 @F
    col1 <- get_gpio @N9  @E
    col2 <- get_gpio @N15 @F
    gpio_init col0 colCfg
    gpio_init col1 colCfg
    gpio_init col2 colCfg

    -- release the keypad to the nonsecure domain -- the whole scan loop runs there.
    -- The gate and lockout LED are NOT released: only door_unlock_attempt may touch them.
    gpio_release row0
    gpio_release row1
    gpio_release row2
    gpio_release row3
    gpio_release col0
    gpio_release col1
    gpio_release col2

    db <- udb_init

    stateRefAction <- initialNSRef initialKeypadState

    lock_configuration

    -- the only thing the nonsecure world may ever do with the lock
    unlockFn <- callable $ door_unlock_attempt db gate lockoutLed

    irq_enable

    nonsecure $ runKeypad uart stateRefAction unlockFn row0 row1 row2 row3 col0 col1 col2

main :: IO ()
main = runSetup app
