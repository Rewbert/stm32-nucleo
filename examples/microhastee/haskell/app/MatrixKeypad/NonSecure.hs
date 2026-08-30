module MatrixKeypad.NonSecure where

import MicroHasTEE

import MatrixKeypad.Secure

-- * Nonsecure-side keypad ----------------------------------------------------

-- ** Type definitions

-- Board wiring (nonsecure side):
-- Keypad (4x3 matrix, same as examples/matrix-keypad/main.c):
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
-- reflects the release order in `app` (UART, then rows, then cols),
-- each release prepending to the list.
type NonsecureEffects =
    Cons COL2_GPIO (Cons COL1_GPIO (Cons COL0_GPIO (
    Cons ROW3_GPIO (Cons ROW2_GPIO (Cons ROW1_GPIO (Cons ROW0_GPIO (
    Cons UART Nil)))))))

-- ** Key scanning 

-- | Position of a pressed key within the matrix, as 0-indexed (row, column)
data KeyPos = KeyPos Int Int

-- | Drive one row low, sample the three columns, restore the row high, and
-- report which column (if any) is pressed in that row.
scanRow :: Member (GPIO pin port) NonsecureEffects
        => GPIO pin port -> COL0_GPIO -> COL1_GPIO -> COL2_GPIO
        -> Nonsecure NonsecureEffects (Maybe Int)
scanRow rowGpio col0 col1 col2 = do
    gpio_write rowGpio False
    c0 <- gpio_read col0
    c1 <- gpio_read col1
    c2 <- gpio_read col2
    gpio_write rowGpio True
    return $ if not c0 then Just 0
             else if not c1 then Just 1
             else if not c2 then Just 2
             else Nothing

infixl 3 <|>

-- | Run the first scan; only run the second if the first found no key. Unlike
-- combining two already-computed 'Maybe's, this short-circuits the FFI calls
-- themselves -- a later row is never driven if an earlier one already hit.
(<|>) :: Nonsecure effects (Maybe a) -> Nonsecure effects (Maybe a)
      -> Nonsecure effects (Maybe a)
mx <|> my = do
    x <- mx
    case x of
        Just _  -> return x
        Nothing -> my

-- | Scans the matrix row by row, stopping as soon as a key is found, and
-- reports its position. Resolving that position to a symbol, or deciding
-- what to do about it, is left to the caller.
scanMatrix :: ROW0_GPIO -> ROW1_GPIO -> ROW2_GPIO -> ROW3_GPIO
           -> COL0_GPIO -> COL1_GPIO -> COL2_GPIO
           -> Nonsecure NonsecureEffects (Maybe KeyPos)
scanMatrix row0 row1 row2 row3 col0 col1 col2 =
       fmap (fmap (KeyPos 0)) (scanRow row0 col0 col1 col2)
   <|> fmap (fmap (KeyPos 1)) (scanRow row1 col0 col1 col2)
   <|> fmap (fmap (KeyPos 2)) (scanRow row2 col0 col1 col2)
   <|> fmap (fmap (KeyPos 3)) (scanRow row3 col0 col1 col2)

-- **

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

-- | Maps a key's matrix position to the symbol printed on it.
keySymbol :: KeyPos -> Char
keySymbol (KeyPos 0 0) = '1'
keySymbol (KeyPos 0 1) = '2'
keySymbol (KeyPos 0 2) = '3'
keySymbol (KeyPos 1 0) = '4'
keySymbol (KeyPos 1 1) = '5'
keySymbol (KeyPos 1 2) = '6'
keySymbol (KeyPos 2 0) = '7'
keySymbol (KeyPos 2 1) = '8'
keySymbol (KeyPos 2 2) = '9'
keySymbol (KeyPos 3 0) = '*'
keySymbol (KeyPos 3 1) = '0'
keySymbol (KeyPos 3 2) = '#'
keySymbol (KeyPos _ _) = ' '

-- | A key was just entered, and now we figure out whether we timed out, whether
-- a key event was a dude, or whether we have contributed to entering a code.
-- The returned KeypadEvent indicates whether the code was fully saturated or not
stepKeypad :: Int -> Maybe Char -> KeypadState -> (KeypadState, KeypadEvent)
stepKeypad now raw (code, lastKey, lastPress)
    | not (null code) && (now - lastPress) > 5000 =
        (([], raw, now), TimedOut)
    | raw == lastKey =
        ((code, raw, lastPress), NoEvent)
    | otherwise = case raw of
        Nothing -> ((code, raw, lastPress), NoEvent)
        Just c ->
            let code' = code ++ [c]
            in if length code' >= 4
                then (([], raw, now), CodeComplete code')
                else ((code', raw, now), NoEvent)

reportResult :: UART -> UnlockResult -> Nonsecure NonsecureEffects ()
reportResult uart Granted            = uart_write uart "unlock granted\r\n"
reportResult uart (Denied remaining) = uart_write uart ("wrong pin, " ++ show remaining ++ " attempt(s) left\r\n")
reportResult uart LockedOut          = uart_write uart "locked out\r\n"

-- | Scans the matrix, advances the keypad state machine, and loops forever.
-- A completed code is sent across the secure gateway ('unlockFn') for
-- verification; a timeout is just reported locally.
keypadLoop :: UART -> NSRef KeypadState -> Callable ([Char] -> Secure SecureEffects UnlockResult)
           -> ROW0_GPIO -> ROW1_GPIO -> ROW2_GPIO -> ROW3_GPIO
           -> COL0_GPIO -> COL1_GPIO -> COL2_GPIO
           -> Nonsecure NonsecureEffects ()
keypadLoop uart stateRef unlockFn row0 row1 row2 row3 col0 col1 col2 = do
    -- read a key and turn it into its representative symbol
    pos <- scanMatrix row0 row1 row2 row3 col0 col1 col2
    let raw = fmap keySymbol pos

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

    keypadLoop uart stateRef unlockFn row0 row1 row2 row3 col0 col1 col2

-- The Non-secure 'main'
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
    keypadLoop uart stateRef unlockFn row0 row1 row2 row3 col0 col1 col2


{-@

type ROW0 = GPIO 11 E
type ROW1 = GPIO 8  G
type ROW2 = GPIO 7  G
type ROW3 = GPIO 13 E
type COL0 = GPIO 14 F
type COL1 = GPIO 9  E
type COL2 = GPIO 15 F

type NonsecureEffects = '[ COL2, COL1, COL0
                         , ROW3, ROW2, ROW1
                         , ROW0, UART]

scanRow :: Member (GPIO pin port) NonsecureEffects
        => GPIO pin port -> COL0 -> COL1 -> COL2
        -> Nonsecure NonsecureEffects (Maybe Int)
scanRow rowGpio col0 col1 col2 = do
    gpio_write rowGpio False
    c0 <- gpio_read col0
    c1 <- gpio_read col1
    c2 <- gpio_read col2
    gpio_write rowGpio True
    return $ if not c0 then Just 0
             else if not c1 then Just 1
             else if not c2 then Just 2
             else Nothing

data KeyPos = KeyPos Int Int

scanMatrix :: ROW0 -> ROW1 -> ROW2 -> ROW3
           -> COL0 -> COL1 -> COL2
           -> Nonsecure NonsecureEffects (Maybe KeyPos)
scanMatrix row0 row1 row2 row3 col0 col1 col2 =
       fmap (fmap (KeyPos 0)) (scanRow row0 col0 col1 col2)
   <|> fmap (fmap (KeyPos 1)) (scanRow row1 col0 col1 col2)
   <|> fmap (fmap (KeyPos 2)) (scanRow row2 col0 col1 col2)
   <|> fmap (fmap (KeyPos 3)) (scanRow row3 col0 col1 col2)

@-}