{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE CPP #-}
module MatrixKeypad where

import Control.DeepSeq (NFData (..))

import qualified Control.Monad.IxMonad as Ix
import Control.Monad.IO.Class
import MicroHasTEE

#ifdef SECURE
foreign export ccall "app_main" main :: IO ()
#endif

-- Board wiring.
-- Keypad (nonsecure, 4x3 matrix, same as examples/matrix-keypad/main.c):
-- D0 = PG8  = row 1   D4 = PF14 = col 0
-- D1 = PG7  = row 2   D5 = PE11 = row 0
-- D2 = PF15 = col 2   D6 = PE9  = col 1
-- D3 = PE13 = row 3
-- Solenoid latch (secure): Arduino A2 = PC3, drives the MOSFET gate
-- Lockout indicator (secure): red LED, PG2.
type ROW0_GPIO = GPIO N11 E
type ROW1_GPIO = GPIO N8  G
type ROW2_GPIO = GPIO N7  G
type ROW3_GPIO = GPIO N13 E
type COL0_GPIO = GPIO N14 F
type COL1_GPIO = GPIO N9  E
type COL2_GPIO = GPIO N15 F
type GATE_GPIO = GPIO N3 C
type LOCKOUT_LED = GPIO N2 G

-- This is the final security attribution for the Non-secure domain. Order
-- reflects the release order in `app` below (UART, then rows, then cols),
-- each release prepending to the list.
type NonsecureEffects =
    Cons COL2_GPIO (Cons COL1_GPIO (Cons COL0_GPIO (
    Cons ROW3_GPIO (Cons ROW2_GPIO (Cons ROW1_GPIO (Cons ROW0_GPIO (
    Cons UART Nil)))))))

type InitialSecure = Cons Unlocked Nil

-- The gate and the lockout LED are never released -- only the secure gateway
-- (door_unlock_attempt) may ever touch them.
type SecureEffects = Cons Locked (Cons LOCKOUT_LED (Cons GATE_GPIO Nil))

-- * Secure-side lock logic ----------------------------------------------------

maxAttempts :: Int
maxAttempts = 3

-- | How long the gate is driven, in ms -- matches UNLOCK_MS in
-- examples/solenoid-test/main.c.
unlockMs :: Int
unlockMs = 3000

factoryPin :: [Char]
factoryPin = "1234"

pinKey :: String
pinKey = "pin"

lockoutKey :: String
lockoutKey = "lockout"

data UnlockResult
    = Granted
    | Denied Int   -- attempts remaining
    | LockedOut
    deriving (Show, Eq)

-- crosses the sg boundary as the result of door_unlock_attempt, so it needs an
-- NFData instance
instance NFData UnlockResult where
    rnf Granted     = ()
    rnf (Denied n)  = rnf n
    rnf LockedOut   = ()

readPin :: UDB -> Secure effects [Char]
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

setLockoutIndicator :: Member LOCKOUT_LED effects => LOCKOUT_LED -> Bool -> Secure effects ()
setLockoutIndicator lockoutLed = gpio_write lockoutLed

-- | Drives the solenoid gate for 'unlockMs', then releases it.
grantAccess :: Member GATE_GPIO effects => GATE_GPIO -> Secure effects ()
grantAccess gate = do
    gpio_write gate True
    systick_delay_ms unlockMs
    gpio_write gate False

-- Records a failed attempt: bumps the persisted counter and, if that trips
-- the threshold, engages the lockout indicator. Returns the new count.
consumeAttempt :: Member LOCKOUT_LED effects => UDB -> LOCKOUT_LED -> Int -> Secure effects Int
consumeAttempt db lockoutLed count = do
    let count' = count + 1
    writeLockoutCount db count'
    if count' >= maxAttempts
        then setLockoutIndicator lockoutLed True
        else return ()
    return count'

-- The PIN comparison, counter update and actuation for a non-locked-out
-- attempt. Ordinary secure code (not NSC). door_unlock_attempt below handles
-- the lockout gate itself and only reaches here once it knows the attempt is
-- allowed to proceed.
verifyAttempt :: (Member GATE_GPIO effects, Member LOCKOUT_LED effects)
              => UDB -> GATE_GPIO -> LOCKOUT_LED -> Int -> [Char] -> Secure effects UnlockResult
verifyAttempt db gate lockoutLed count attempt = do
    pin <- readPin db
    if attempt == pin
        then do
            writeLockoutCount db 0
            setLockoutIndicator lockoutLed False
            grantAccess gate
            return Granted
        else do
            count' <- consumeAttempt db lockoutLed count
            return (Denied (maxAttempts - count'))

door_unlock_attempt :: (Member GATE_GPIO effects, Member LOCKOUT_LED effects)
                     => UDB -> GATE_GPIO -> LOCKOUT_LED -> [Char] -> Secure effects UnlockResult
door_unlock_attempt db gate lockoutLed attempt = do
    count <- readLockoutCount db
    if count >= maxAttempts
        then do
            setLockoutIndicator lockoutLed True
            return LockedOut
        else verifyAttempt db gate lockoutLed count attempt

-- * Nonsecure-side keypad ----------------------------------------------------

-- | Drive one row low, sample the three columns, restore the row high, and
-- report which key (if any) that row's press corresponds to.
scanRow :: Member (GPIO pin port) NonsecureEffects
        => GPIO pin port -> COL0_GPIO -> COL1_GPIO -> COL2_GPIO
        -> Char -> Char -> Char -> Nonsecure NonsecureEffects (Maybe Char)
scanRow rowGpio col0 col1 col2 k0 k1 k2 = do
    gpio_write rowGpio False
    c0 <- gpio_read col0
    c1 <- gpio_read col1
    c2 <- gpio_read col2
    gpio_write rowGpio True
    return $ if not c0 then Just k0
             else if not c1 then Just k1
             else if not c2 then Just k2
             else Nothing

firstPressed :: [Maybe Char] -> Maybe Char
firstPressed [] = Nothing
firstPressed (Just c : _) = Just c
firstPressed (Nothing : rest) = firstPressed rest

-- | Avoids relying on an Eq (Maybe Char) instance, which MicroHs's Prelude
-- subset may not provide -- only Eq Char is needed here.
sameKey :: Maybe Char -> Maybe Char -> Bool
sameKey Nothing Nothing = True
sameKey (Just a) (Just b) = a == b
sameKey _ _ = False

codeLength :: Int
codeLength = 4

-- | Max ticks (systick fires once per ms) allowed to pass between two
-- presses of a code in progress before it's abandoned.
timeoutTicks :: Int
timeoutTicks = 5000

-- | (code collected so far, last raw scan result -- for edge detection,
-- tick of the last accepted press -- for the timeout). Lives in one NSRef
-- so the scan loop below carries no state of its own.
type KeypadState = ([Char], Maybe Char, Int)

initialKeypadState :: KeypadState
initialKeypadState = ([], Nothing, 0)

-- | What 'stepKeypad' decides happened on a given scan.
data KeypadEvent
    = NoEvent
    | TimedOut
    | CodeComplete [Char]

-- | Pure decision step, kept separate from the Nonsecure/IO shell: given the
-- current tick count, this scan's raw key reading, and the current state,
-- decides the next state and what (if anything) happened.
--
--   * a key is accepted exactly once, on the scan where it first reads as
--     pressed (comparing against the previous scan's result) -- not via
--     timing-based debounce;
--   * once 'codeLength' keys have been accepted, 'CodeComplete' carries the
--     finished code and the buffer starts over;
--   * if the buffer is non-empty and more than 'timeoutTicks' pass between
--     accepted presses, the buffer is discarded and 'TimedOut' is reported.
stepKeypad :: Int -> Maybe Char -> KeypadState -> (KeypadState, KeypadEvent)
stepKeypad now raw (code, lastKey, lastPress)
    | not (null code) && (now - lastPress) > timeoutTicks =
        (([], raw, now), TimedOut)
    | sameKey raw lastKey =
        ((code, raw, lastPress), NoEvent)
    | otherwise = case raw of
        Nothing -> ((code, raw, lastPress), NoEvent)
        Just c ->
            let code' = code ++ [c]
            in if length code' >= codeLength
                then (([], raw, now), CodeComplete code')
                else ((code', raw, now), NoEvent)

reportResult :: UART -> UnlockResult -> Nonsecure NonsecureEffects ()
reportResult uart Granted            = uart_write uart "unlock granted\r\n"
reportResult uart (Denied remaining) = uart_write uart ("wrong pin, " ++ show remaining ++ " attempt(s) left\r\n")
reportResult uart LockedOut          = uart_write uart "locked out\r\n"

-- | Scans the whole matrix once, advances the keypad state machine, and
-- loops forever. A completed code is sent across the secure gateway
-- ('unlockFn') for verification; a timeout is just reported locally.
scanMatrix :: UART -> NSRef KeypadState -> Callable ([Char] -> Secure SecureEffects UnlockResult)
           -> ROW0_GPIO -> ROW1_GPIO -> ROW2_GPIO -> ROW3_GPIO
           -> COL0_GPIO -> COL1_GPIO -> COL2_GPIO
           -> Nonsecure NonsecureEffects ()
scanMatrix uart stateRef unlockFn row0 row1 row2 row3 col0 col1 col2 = do
    k0 <- scanRow row0 col0 col1 col2 '1' '2' '3'
    k1 <- scanRow row1 col0 col1 col2 '4' '5' '6'
    k2 <- scanRow row2 col0 col1 col2 '7' '8' '9'
    k3 <- scanRow row3 col0 col1 col2 '*' '0' '#'
    let raw = firstPressed [k0, k1, k2, k3]

    now <- systick_ticks
    st <- readNSRef stateRef
    let (st', event) = stepKeypad now raw st
    writeNSRef stateRef st'
    case event of
        NoEvent -> return ()
        TimedOut -> uart_write uart "timeout: reset state\r\n"
        CodeComplete code -> do
            result <- sg (unlockFn <.> code)
            reportResult uart result

    scanMatrix uart stateRef unlockFn row0 row1 row2 row3 col0 col1 col2

-- | Rows idle high; each is driven low in turn to scan. Set once, then hand
-- off to the scan loop. Takes the ref-creating action returned by
-- 'initialNSRef' (run here, once) rather than an already-created ref, so
-- 'app' below doesn't need a nested plain 'do' inside its 'Ix.do' block.
runKeypad :: UART -> Nonsecure NonsecureEffects (NSRef KeypadState) -> Callable ([Char] -> Secure SecureEffects UnlockResult)
          -> ROW0_GPIO -> ROW1_GPIO -> ROW2_GPIO -> ROW3_GPIO
          -> COL0_GPIO -> COL1_GPIO -> COL2_GPIO
          -> Nonsecure NonsecureEffects ()
runKeypad uart stateRefAction unlockFn row0 row1 row2 row3 col0 col1 col2 = do
    stateRef <- stateRefAction
    gpio_write row0 True
    gpio_write row1 True
    gpio_write row2 True
    gpio_write row3 True
    scanMatrix uart stateRef unlockFn row0 row1 row2 row3 col0 col1 col2

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
