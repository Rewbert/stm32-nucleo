module MatrixKeypad.Secure where

import Control.DeepSeq (NFData (..))

import MicroHasTEE

-- * Secure-side lock logic ----------------------------------------------------

-- ** Type definitions

-- Board wiring (secure side):
-- Solenoid latch: Arduino A2 = PC3, drives the MOSFET gate.
-- Lockout indicator: red LED, PG2.
type GATE_GPIO = GPIO N3 C
type LOCKOUT_LED = GPIO N2 G

type InitialSecure = Cons Unlocked Nil

-- The gate and the lockout LED are never released -- only the secure gateway
-- (door_unlock_attempt) may ever touch them.
type SecureEffects = Cons Locked (Cons LOCKOUT_LED (Cons GATE_GPIO Nil))

-- ** Constants

maxAttempts :: Int
maxAttempts = 3

-- | How long the gate is driven, in ms -- matches UNLOCK_MS in
-- examples/solenoid-test/main.c.
unlockMs :: Int
unlockMs = 3000

pinKey :: String
pinKey = "pin"

lockoutKey :: String
lockoutKey = "lockout"

-- ** Database management

readPin :: UDB -> Secure effects [Char]
readPin db = do
    mp <- udb_lookup db pinKey
    case mp of
        Just p  -> return p
        Nothing -> do
            udb_insert db pinKey "1234"
            return "1234"

readLockoutCount :: UDB -> Secure effects Int
readLockoutCount db = do
    mc <- udb_lookup db lockoutKey
    case mc of
        Just c  -> return c
        Nothing -> return 0

writeLockoutCount :: UDB -> Int -> Secure effects ()
writeLockoutCount db c = udb_insert db lockoutKey c

-- ** Secure peripheral operations

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

-- ** Attempt an unlock

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

-- ** NSC Functions

door_unlock_attempt :: (Member GATE_GPIO effects, Member LOCKOUT_LED effects)
                     => UDB -> GATE_GPIO -> LOCKOUT_LED -> [Char] -> Secure effects UnlockResult
door_unlock_attempt db gate lockoutLed attempt = do
    count <- readLockoutCount db
    if count >= maxAttempts
        then do
            setLockoutIndicator lockoutLed True
            return LockedOut
        else verifyAttempt db gate lockoutLed count attempt
