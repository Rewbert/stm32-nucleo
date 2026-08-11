{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE CPP #-}
module DoorLock where

import Data.Proxy

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

-- Board wiring (see firmware/boards/stm32u5/board.c): PC7 = green, PB7 = blue,
-- PG2 = red. Keypad is 4 free GPIOs (PD0..PD3) -- no fixed function yet, to be
-- hard-wired to a breadboard later. Each button is one digit (1-4) of the PIN.
type DOORLOCKED_LED   = GPIO N2 G
type DOORUNLOCKED_LED = GPIO N7 C
type LOCKOUT_LED      = GPIO N7 B
type KEY1_GPIO = GPIO N0 D
type KEY2_GPIO = GPIO N1 D
type KEY3_GPIO = GPIO N2 D
type KEY4_GPIO = GPIO N3 D
type KEY1_EXTI = EXTI N0 D
type KEY2_EXTI = EXTI N1 D
type KEY3_EXTI = EXTI N2 D
type KEY4_EXTI = EXTI N3 D

-- released in GPIO/EXTI pairs, key4 down to key1 -- see the note above on why
-- interleaving (rather than 4 gpio_release in a row) is what actually compiles
type NonsecureEffects =
    Cons KEY4_EXTI (Cons KEY4_GPIO (
    Cons KEY3_EXTI (Cons KEY3_GPIO (
    Cons KEY2_EXTI (Cons KEY2_GPIO (
    Cons KEY1_EXTI (Cons KEY1_GPIO (
    Cons UART Nil))))))))

type InitialSecure = Cons Unlocked Nil
-- [LOCKOUT_LED, DOORUNLOCKED_LED, DOORLOCKED_LED, Unlocked] -- s while the keypad
-- GPIOs/EXTIs are acquired and immediately released, one pair at a time
type PreLockSecure = Cons LOCKOUT_LED (Cons DOORUNLOCKED_LED (Cons DOORLOCKED_LED InitialSecure))
-- the three LEDs, still held securely once configuration is complete -- door_unlock_attempt
-- is the only thing that will ever drive them
type ConfiguredSecure = Cons LOCKOUT_LED (Cons DOORUNLOCKED_LED (Cons DOORLOCKED_LED Nil))
-- app's final state: locked, holding exactly the configured peripherals
type SecureEffects = Cons Locked ConfiguredSecure

-- * Secure-side lock logic -------------------------------------------------

maxAttempts :: Int
maxAttempts = 3

factoryPin :: [Int]
factoryPin = [1, 2, 3, 4]

pinKey :: String
pinKey = "pin"

lockoutKey :: String
lockoutKey = "lockout"

data UnlockResult
    = Collecting Int   -- digits received so far, out of 4
    | Granted
    | Denied Int       -- attempts remaining
    | LockedOut
    deriving (Show, Eq)

readPin :: UDB -> Secure effects [Int]
readPin db = do
    mp <- udb_lookup db pinKey
    case mp of
        Just p  -> return p
        Nothing -> do
            udb_insert db pinKey factoryPin
            return factoryPin

readLockoutCount :: UDB -> Secure effects Int
readLockoutCount db = do
    mc <- udb_lookup db lockoutKey
    case mc of
        Just c  -> return c
        Nothing -> return 0

writeLockoutCount :: UDB -> Int -> Secure effects ()
writeLockoutCount db c = udb_insert db lockoutKey c

door_unlock_attempt :: (Member DOORLOCKED_LED effects, Member DOORUNLOCKED_LED effects, Member LOCKOUT_LED effects)
                     => UDB -> DOORLOCKED_LED -> DOORUNLOCKED_LED -> LOCKOUT_LED
                     -> [Int] -> Secure effects UnlockResult
door_unlock_attempt db lockedLed unlockedLed lockoutLed attempt = do
    count <- readLockoutCount db
    if count >= maxAttempts
        then do
            gpio_write lockoutLed True
            return LockedOut
        else do
            pin <- readPin db
            if attempt == pin
                then do
                    writeLockoutCount db 0
                    gpio_write lockoutLed False
                    gpio_write lockedLed False
                    gpio_write unlockedLed True
                    systick_delay_ms 2000   -- momentary unlock, like an electric strike
                    gpio_write unlockedLed False
                    gpio_write lockedLed True
                    return Granted
                else do
                    let count' = count + 1
                    writeLockoutCount db count'
                    if count' >= maxAttempts
                        then gpio_write lockoutLed True
                        else return ()
                    return (Denied (maxAttempts - count'))

-- * Nonsecure-side keypad ---------------------------------------------------

reportResult :: UART -> UnlockResult -> Nonsecure NonsecureEffects ()
reportResult uart (Collecting n) =
    uart_write uart ("key " ++ show n ++ "/4\r\n")
reportResult uart Granted =
    uart_write uart "door: unlock granted\r\n"
reportResult uart (Denied remaining) =
    uart_write uart ("door: wrong pin, " ++ show remaining ++ " attempt(s) left\r\n")
reportResult uart LockedOut =
    uart_write uart "door: locked out\r\n"

key_pressed :: UART -> Callable ([Int] -> Secure SecureEffects UnlockResult)
            -> Nonsecure NonsecureEffects (NSRef [Int]) -> Int -> EXTIEdge -> Nonsecure NonsecureEffects ()
key_pressed uart unlockFn bufferRefAction digit _edge = do
    bufferRef <- bufferRefAction
    buf <- readNSRef bufferRef
    let buf' = buf ++ [digit]
    if length buf' < 4
        then do
            writeNSRef bufferRef buf'
            reportResult uart (Collecting (length buf'))
        else do
            writeNSRef bufferRef []
            result <- sg (unlockFn <.> buf')
            reportResult uart result

-- | Keeps main() from ever returning (see the note above on why that matters) now
-- that all real work happens in EXTI callbacks -- there is no polling loop left.
idleLoop :: Nonsecure NonsecureEffects ()
idleLoop = systick_delay_ms 1000 >> idleLoop

-- * Setup --------------------------------------------------------------------

setupKeypadButton :: forall pin port ns s .
                      ( ToInt pin, ToGPIOPort port, Member Unlocked s
                      , Fresh (GPIO pin port) s, Fresh (GPIO pin port) ns
                      , Fresh (EXTI pin port) s, Fresh (EXTI pin port) ns
                      )
                   => GPIOConfig -> EXTIEdge -> Int
                   -> Setup ns s (Cons (EXTI pin port) (Cons (GPIO pin port) ns)) s (EXTI pin port, Int)
setupKeypadButton inCfg edge priority = Ix.do
    gpio <- get_gpio @pin @port
    gpio_init gpio inCfg
    gpio_release @s gpio
    exti <- get_exti @pin @port
    exti_init exti $ EXTIConfig { port = toPort (Proxy :: Proxy port), pin = toInt (Proxy :: Proxy pin), edge = edge }
    irqn <- exti_irqn exti
    nvic_set_priority irqn priority
    exti_release @s exti
    Ix.return (exti, irqn)

app :: Setup Nil InitialSecure NonsecureEffects SecureEffects ()
app = Ix.do
    -- init board and configure frequency
    board_init
    board_configure_pll

    -- set systick handler to fire every ms
    hz <- board_sysclk_hz
    systick_configure (hz `div` 1000)

    -- configure UART and release it to the nonsecure domain
    uart <- get_console
    uart_init uart $ UARTConfig { baudrate = 115200, word_length = 8, stop_bits = 1, parity = NONE }
    tzsc <- get_tzsc
    tzsc_release_periph @InitialSecure tzsc uart

    -- enable GPIO ports (A/C for UART/green, B for blue, G for red, D for the keypad)
    rcc <- get_rcc
    rcc_enable rcc RCC_GPIOA
    rcc_enable rcc RCC_GPIOB
    rcc_enable rcc RCC_GPIOC
    rcc_enable rcc RCC_GPIOD

    let outCfg = GPIOConfig { mode = OUTPUT, pull = NOPULL,   alternate = AF0 }
        inCfg  = GPIOConfig { mode = INPUT,  pull = PULLDOWN, alternate = AF0 }

    lockedLed   <- get_gpio @N2 @G
    unlockedLed <- get_gpio @N7 @C
    lockoutLed  <- get_gpio @N7 @B
    gpio_init lockedLed   outCfg
    gpio_init unlockedLed outCfg
    gpio_init lockoutLed  outCfg

    db <- udb_init

    bufferRefAction <- initialNSRef ([] :: [Int])

    (key1x, irqn1) <- setupKeypadButton @N0 @D inCfg RISING 0
    (key2x, irqn2) <- setupKeypadButton @N1 @D inCfg RISING 0
    (key3x, irqn3) <- setupKeypadButton @N2 @D inCfg RISING 0
    (key4x, irqn4) <- setupKeypadButton @N3 @D inCfg RISING 0

    lock_configuration

    -- the only thing the nonsecure world may ever do with the lock
    unlockFn <- callable $ door_unlock_attempt db lockedLed unlockedLed lockoutLed

    -- register a callback per key, then arm all four interrupts
    exti_on_nonsecure key1x RISING $ key_pressed uart unlockFn bufferRefAction 1
    exti_on_nonsecure key2x RISING $ key_pressed uart unlockFn bufferRefAction 2
    exti_on_nonsecure key3x RISING $ key_pressed uart unlockFn bufferRefAction 3
    exti_on_nonsecure key4x RISING $ key_pressed uart unlockFn bufferRefAction 4
    nvic_enable_irq irqn1
    nvic_enable_irq irqn2
    nvic_enable_irq irqn3
    nvic_enable_irq irqn4
    irq_enable

    -- everything now happens in the EXTI callbacks above; just keep main() alive
    nonsecure idleLoop

main :: IO ()
main = runSetup app
