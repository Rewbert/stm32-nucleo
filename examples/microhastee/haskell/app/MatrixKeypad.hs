{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE CPP #-}
module MatrixKeypad where

import qualified Control.Monad.IxMonad as Ix
import Control.Monad.IO.Class
import MicroHasTEE

#ifdef SECURE
foreign export ccall "app_main" main :: IO ()
#endif

-- D0 = PG8  = row 1   D4 = PF14 = col 0
-- D1 = PG7  = row 2   D5 = PE11 = row 0
-- D2 = PF15 = col 2   D6 = PE9  = col 1
-- D3 = PE13 = row 3
type ROW0_GPIO = GPIO N11 E
type ROW1_GPIO = GPIO N8  G
type ROW2_GPIO = GPIO N7  G
type ROW3_GPIO = GPIO N13 E
type COL0_GPIO = GPIO N14 F
type COL1_GPIO = GPIO N9  E
type COL2_GPIO = GPIO N15 F

-- This is the final security attribution for the Non-secure domain. Order
-- reflects the release order in `app` below (UART, then rows, then cols),
-- each release prepending to the list.
type NonsecureEffects =
    Cons COL2_GPIO (Cons COL1_GPIO (Cons COL0_GPIO (
    Cons ROW3_GPIO (Cons ROW2_GPIO (Cons ROW1_GPIO (Cons ROW0_GPIO (
    Cons UART Nil)))))))

type InitialSecure = Cons Unlocked Nil

-- Everything is released to the nonsecure domain, so nothing remains secure.
type SecureEffects = Cons Locked Nil

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

-- | Pure decision step, kept separate from the Nonsecure/IO shell: given the
-- current tick count, this scan's raw key reading, and the current state,
-- decides the next state and (if any) the line to print.
--
--   * a key is accepted exactly once, on the scan where it first reads as
--     pressed (comparing against the previous scan's result) -- not via
--     timing-based debounce;
--   * once 'codeLength' keys have been accepted, the code is emitted and
--     the buffer starts over;
--   * if the buffer is non-empty and more than 'timeoutTicks' pass between
--     accepted presses, the buffer is discarded and a timeout is reported.
stepKeypad :: Int -> Maybe Char -> KeypadState -> (KeypadState, Maybe String)
stepKeypad now raw (code, lastKey, lastPress)
    | not (null code) && (now - lastPress) > timeoutTicks =
        (([], raw, now), Just "timeout: reset state\r\n")
    | sameKey raw lastKey =
        ((code, raw, lastPress), Nothing)
    | otherwise = case raw of
        Nothing -> ((code, raw, lastPress), Nothing)
        Just c ->
            let code' = code ++ [c]
            in if length code' >= codeLength
                then (([], raw, now), Just ("entered code: " ++ code' ++ "\r\n"))
                else ((code', raw, now), Nothing)

-- | Scans the whole matrix once, advances the keypad state machine, and
-- loops forever.
scanMatrix :: UART -> NSRef KeypadState
           -> ROW0_GPIO -> ROW1_GPIO -> ROW2_GPIO -> ROW3_GPIO
           -> COL0_GPIO -> COL1_GPIO -> COL2_GPIO
           -> Nonsecure NonsecureEffects ()
scanMatrix uart stateRef row0 row1 row2 row3 col0 col1 col2 = do
    k0 <- scanRow row0 col0 col1 col2 '1' '2' '3'
    k1 <- scanRow row1 col0 col1 col2 '4' '5' '6'
    k2 <- scanRow row2 col0 col1 col2 '7' '8' '9'
    k3 <- scanRow row3 col0 col1 col2 '*' '0' '#'
    let raw = firstPressed [k0, k1, k2, k3]

    now <- systick_ticks
    st <- readNSRef stateRef
    let (st', msg) = stepKeypad now raw st
    writeNSRef stateRef st'
    case msg of
        Nothing -> return ()
        Just m  -> uart_write uart m

    scanMatrix uart stateRef row0 row1 row2 row3 col0 col1 col2

-- | Rows idle high; each is driven low in turn to scan. Set once, then hand
-- off to the scan loop. Takes the ref-creating action returned by
-- 'initialNSRef' (run here, once) rather than an already-created ref, so
-- 'app' below doesn't need a nested plain 'do' inside its 'Ix.do' block.
runKeypad :: UART -> Nonsecure NonsecureEffects (NSRef KeypadState)
          -> ROW0_GPIO -> ROW1_GPIO -> ROW2_GPIO -> ROW3_GPIO
          -> COL0_GPIO -> COL1_GPIO -> COL2_GPIO
          -> Nonsecure NonsecureEffects ()
runKeypad uart stateRefAction row0 row1 row2 row3 col0 col1 col2 = do
    stateRef <- stateRefAction
    gpio_write row0 True
    gpio_write row1 True
    gpio_write row2 True
    gpio_write row3 True
    scanMatrix uart stateRef row0 row1 row2 row3 col0 col1 col2

-- * Setup ---------------------------------------------------------------------

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
    tzsc_release_periph tzsc uart

    -- enable GPIO ports used by the row/column pins
    rcc <- get_rcc
    rcc_enable rcc RCC_GPIOE
    rcc_enable rcc RCC_GPIOF
    rcc_enable rcc RCC_GPIOG

    let rowCfg = GPIOConfig { mode = OUTPUT, pull = NOPULL,  alternate = AF0 }
        colCfg = GPIOConfig { mode = INPUT,  pull = PULLUP,  alternate = AF0 }

    row0 <- get_gpio @N11 @E
    row1 <- get_gpio @N8  @G
    row2 <- get_gpio @N7  @G
    row3 <- get_gpio @N13 @E
    gpio_init row0 rowCfg
    gpio_init row1 rowCfg
    gpio_init row2 rowCfg
    gpio_init row3 rowCfg

    col0 <- get_gpio @N14 @F
    col1 <- get_gpio @N9  @E
    col2 <- get_gpio @N15 @F
    gpio_init col0 colCfg
    gpio_init col1 colCfg
    gpio_init col2 colCfg

    -- release everything to the nonsecure domain -- the whole scan loop runs there
    gpio_release row0
    gpio_release row1
    gpio_release row2
    gpio_release row3
    gpio_release col0
    gpio_release col1
    gpio_release col2

    stateRefAction <- initialNSRef initialKeypadState

    lock_configuration

    irq_enable

    nonsecure $ runKeypad uart stateRefAction row0 row1 row2 row3 col0 col1 col2

main :: IO ()
main = runSetup app
