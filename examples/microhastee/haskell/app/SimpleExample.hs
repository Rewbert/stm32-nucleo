{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE CPP #-}
module SimpleExample where

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup
import Control.Monad.IO.Class

-- Conditional compilation to include one of two implementations of the same API
#ifdef SECURE
import Effectful.Secure
#else
import Effectful.NonSecure
#endif

import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.TypeLevel.Lock
import Effectful.HAL

#ifdef SECURE
foreign export ccall "app_main" main :: IO ()
#endif

{-
red   led = G 2
blue  led = B 7
green led = C 7
-}

type REDLED = GPIO N2 G
type GREENLED = GPIO N7 C
type BLUELED = GPIO N7 B
type USER_BUTTON_GPIO = GPIO N13 C
type USER_BUTTON_EXTI = EXTI N13 C

-- [EXTI 13 C, GPIO 13 C, GPIO 7 C, UART]
type NonsecureEffects = Cons USER_BUTTON_EXTI (Cons USER_BUTTON_GPIO (Cons GREENLED (Cons UART Nil)))

-- every Setup computation starts holding Unlocked; lock_configuration is the only
-- thing that ever removes it, so this is the ledger state before any peripheral is acquired
type InitialSecure = Cons Unlocked Nil
-- [GPIO 2 G, GPIO 7 B, Unlocked] -- s right before lock_configuration is called
type PreLockSecure = Cons REDLED (Cons BLUELED InitialSecure)
-- [GPIO 2 G, GPIO 7 B] -- the peripherals still held securely once configuration is complete
type ConfiguredSecure = Cons REDLED (Cons BLUELED Nil)
-- app's final state: locked, holding exactly the configured peripherals
type SecureEffects    = Cons Locked ConfiguredSecure

secureBlink :: (Member (GPIO pin port) effects)
            => GPIO pin port -> Int -> Int -> Secure effects Int
secureBlink gpio m n = do
    gpio_toggle gpio
    return $ m + n

loop :: (Member UART effects) => UART -> Callable (Int -> Int -> Secure seffects Int) -> Int -> Int -> Nonsecure effects ()
loop uart nsc_f i j = do
    let fullyAppliedF = nsc_f <.> i <.> j
    r <- sg fullyAppliedF

    uart_write uart ("result from secure: " ++ show r ++ "\r\n")
    systick_delay_ms 500
    loop uart nsc_f (i + 1) (j + 10)

nonsecure_button_callback :: ( Member UART effects
                             , Member (GPIO pin port) effects)
                          => EXTIEdge -> UART -> GPIO pin port -> Nonsecure effects ()
nonsecure_button_callback edge uart gpio = do
    gpio_toggle gpio
    uart_write uart $ "button was pressed, and the edge was " ++ show edge ++ "\r\n"

app :: Setup Nil InitialSecure NonsecureEffects SecureEffects ()
app = Ix.do
    -- init board and configure frequency
    board_init
    board_configure_pll

    -- set systick handler to fire every ms
    hz <- board_sysclk_hz
    systick_configure (hz `div` 1000)

    -- configure UART
    uart <- get_console
    uart_init uart $ UARTConfig { baudrate = 115200, word_length = 8, stop_bits = 1, parity = NONE }

    -- release the UART to the nonsecure domain
    tzsc <- get_tzsc
    tzsc_release_periph @InitialSecure tzsc uart

    -- enable GPIO ports
    rcc <- get_rcc
    rcc_enable rcc RCC_GPIOA
    rcc_enable rcc RCC_GPIOB
    rcc_enable rcc RCC_GPIOC

    -- configure secure LEDs
    blue <- get_gpio @N7 @B
    red <- get_gpio @N2 @G
    let cfg = GPIOConfig { mode = OUTPUT, pull = NOPULL, alternate = AF0 }
    gpio_init red cfg
    gpio_init blue cfg

    -- configure nonsecure LED
    green <- get_gpio @N7 @C
    gpio_init green cfg
    gpio_release @PreLockSecure green

    -- configure button GPIO and release to the nonsecure domain
    buttonGpio <- get_gpio @N13 @C
    gpio_init buttonGpio $ GPIOConfig { mode = INPUT, pull = PULLDOWN, alternate = AF0 }
    gpio_release @PreLockSecure buttonGpio

    -- configure user button EXTI for the nonsecure domain
    button <- get_button_exti
    exti_init button $ EXTIConfig { edge = BOTH }
    irqn <- exti_irqn button
    nvic_set_priority irqn 0
    exti_release @PreLockSecure button

    -- configuration is complete: no further security/policy-changing call can type-check
    -- after this point, only callback/callable installation and launching the nonsecure world
    lock_configuration

    -- register the button callback before arming the interrupt that could fire it
    exti_on_nonsecure button BOTH $ \e -> nonsecure_button_callback e uart green
    nvic_enable_irq irqn
    irq_enable

    -- mark secureBlink as callable from the nonsecure domain
    f <- callable $ secureBlink blue -- Setup Nil s ns effects (Callable (Int -> Int -> Secure effects ()))
    -- sg :: Callable (Secure effects a) -> NonSecure a

    -- run the nonsecure application
    nonsecure $ loop uart f 0 0

main :: IO ()
main = runSetup app
