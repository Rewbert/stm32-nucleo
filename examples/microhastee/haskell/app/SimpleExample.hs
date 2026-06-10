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

import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import qualified Effectful.HAL as H

#ifdef SECURE
foreign export ccall "app_main" main :: IO ()
#endif

{-
red   led = G 2
blue  led = B 7
green led = C 7
-}

type REDLED = H.GPIO N2 H.G
type GREENLED = H.GPIO N7 H.C
type BLUELED = H.GPIO N7 H.B
type USER_BUTTON_GPIO = H.GPIO N13 H.C
type USER_BUTTON_EXTI = H.EXTI N13 H.C

-- [EXTI 13 C, GPIO 13 C, GPIO 7 C, UART]
type NonsecureEffects = Cons USER_BUTTON_EXTI (Cons USER_BUTTON_GPIO (Cons GREENLED (Cons H.UART Nil)))
-- [GPIO 2 G, GPIO 7 B]
type SecureEffects    = Cons REDLED (Cons BLUELED Nil)

secureBlink :: (Member (H.GPIO pin port) effects)
            => H.GPIO pin port -> Int -> Int -> Secure effects Int
secureBlink gpio m n = do
    H.gpio_toggle gpio
    return $ m + n

loop :: (Member H.UART effects) => H.UART -> Callable (Int -> Int -> Secure seffects Int) -> Int -> Int -> Nonsecure effects ()
loop uart nsc_f i j = do
    let fullyAppliedF = nsc_f <.> i <.> j
    r <- sg fullyAppliedF

    H.uart_write uart ("result from secure: " ++ show r ++ "\r\n")
    H.systick_delay_ms 500
    loop uart nsc_f (i + 1) (j + 10)

nonsecure_button_callback :: ( Member H.UART effects
                             , Member (H.GPIO pin port) effects)
                          => H.EXTIEdge -> H.UART -> H.GPIO pin port -> Nonsecure effects ()
nonsecure_button_callback edge uart gpio = do
    H.gpio_toggle gpio
    H.uart_write uart $ "button was pressed, and the edge was " ++ show edge ++ "\r\n"

app :: Setup Nil Nil NonsecureEffects SecureEffects ()
app = Ix.do
    -- init board and configure frequency
    H.board_init
    H.board_configure_pll

    -- set systick handler to fire every ms
    hz <- H.board_sysclk_hz
    H.systick_configure (hz `div` 1000)

    -- configure UART
    uart <- H.get_console
    H.uart_init uart $ UARTConfig { baudrate = 115200, word_length = 8, stop_bits = 1, parity = NONE }

    -- release the UART to the nonsecure domain
    tzsc <- H.get_tzsc
    H.tzsc_release_periph @Nil tzsc uart -- commenting out this should make the loop function not work, as the uart effect is not there. However, it still does. FIgure out why.

    -- enable GPIO ports
    rcc <- H.get_rcc
    H.rcc_enable rcc H.RCC_GPIOA
    H.rcc_enable rcc H.RCC_GPIOB
    H.rcc_enable rcc H.RCC_GPIOC

    -- configure secure LEDs
    blue <- H.get_gpio @N7 @H.B
    red <- H.get_gpio @N2 @H.G
    let cfg = H.GPIOConfig { H.mode = OUTPUT, H.pull = NOPULL, H.alternate = AF0 }
    H.gpio_init red cfg
    H.gpio_init blue cfg

    -- configure nonsecure LED
    green <- H.get_gpio @N7 @H.C
    H.gpio_init green cfg
    H.gpio_release @SecureEffects green

    -- configure button GPIO and release to the nonsecure domain
    buttonGpio <- H.get_gpio @N13 @H.C
    H.gpio_init buttonGpio $ H.GPIOConfig { H.mode = INPUT, H.pull = PULLDOWN, H.alternate = AF0 }
    H.gpio_release @SecureEffects buttonGpio
    
    -- configure user button EXTI for the nonsecure domain
    button <- H.get_button_exti
    H.exti_init button $ H.EXTIConfig { H.port = C, H.pin = 13, H.edge = H.BOTH }
    irqn <- H.exti_irqn button
    H.nvic_set_priority irqn 0
    H.exti_release @SecureEffects button
    H.exti_on_nonsecure button H.BOTH $ \e -> nonsecure_button_callback e uart green
    H.nvic_enable_irq irqn

    H.irq_enable

    -- mark secureBlink as callable from the nonsecure domain
    f <- callable $ secureBlink blue -- Setup Nil Nil ns effects (Callable (Int -> Int -> Secure effects ()))
    -- sg :: Callable (Secure effects a) -> NonSecure a

    -- run the nonsecure application
    nonsecure $ loop uart f 0 0

main :: IO ()
main = runSetup app
