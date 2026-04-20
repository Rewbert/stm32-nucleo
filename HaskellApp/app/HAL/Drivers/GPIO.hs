module HAL.Drivers.GPIO (
    -- * GPIO device type
    GPIO,

    -- * GPIO options and configuration
    GPIOSecurity (..),
    GPIOMode (..),
    GPIOPull (..),
    GPIOAF (..),
    GPIOPort (..),
    GPIOConfig (..),

    -- * Operations on GPIO devices
    gpio_init,
    gpio_write,
    gpio_read,
    gpio_toggle,
) where

import Foreign.C.Types (CInt (..))
import Foreign.HAL.Utils
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

-- gpio_config_t is passed as Ptr () (caller fills in C)
foreign import ccall "drivers/gpio.h gpio_init" gpioInit :: GPIO -> Ptr () -> IO ()
foreign import ccall "drivers/gpio.h gpio_write" gpioWrite :: GPIO -> CInt -> IO ()
foreign import ccall "drivers/gpio.h gpio_read" gpioRead :: GPIO -> IO CInt
foreign import ccall "drivers/gpio.h gpio_toggle" gpioToggle :: GPIO -> IO ()

type GPIO = Ptr () -- gpio_dev_t *

-- gpio_level_t
gpioLow :: CInt
gpioLow = 0

gpioHigh :: CInt
gpioHigh = 1

-- gpio_security_t
data GPIOSecurity = GPIOSecure | GPIONonsecure

instance Enum GPIOSecurity where
    fromEnum :: GPIOSecurity -> Int
    fromEnum GPIOSecure = 0
    fromEnum GPIONonsecure = 1

    toEnum :: Int -> GPIOSecurity
    toEnum 0 = GPIOSecure
    toEnum 1 = GPIONonsecure
    toEnum _ = error "GPIOSecurity error: not a valid enum variant"

instance Storable GPIOSecurity where
    sizeOf :: GPIOSecurity -> Int
    sizeOf _ = 4

    alignment :: GPIOSecurity -> Int
    alignment _ = 4

    peek :: Ptr GPIOSecurity -> IO GPIOSecurity
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr GPIOSecurity -> GPIOSecurity -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- gpio_mode_t
data GPIOMode = INPUT | OUTPUT | AF | ANALOG

instance Enum GPIOMode where
    fromEnum INPUT = 0
    fromEnum OUTPUT = 1
    fromEnum AF = 2
    fromEnum ANALOG = 3

    toEnum 0 = INPUT
    toEnum 1 = OUTPUT
    toEnum 2 = AF
    toEnum 3 = ANALOG
    toEnum _ = error "GPIOMode error: not a valid enum variant"

instance Storable GPIOMode where
    sizeOf :: GPIOMode -> Int
    sizeOf _ = 4

    alignment :: GPIOMode -> Int
    alignment _ = 4

    peek :: Ptr GPIOMode -> IO GPIOMode
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr GPIOMode -> GPIOMode -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- gpio_pull_t
data GPIOPull = Nopull | Pullup | Pulldown

instance Enum GPIOPull where
    fromEnum Nopull = 0
    fromEnum Pullup = 1
    fromEnum Pulldown = 2

    toEnum 0 = Nopull
    toEnum 1 = Pullup
    toEnum 2 = Pulldown
    toEnum _ = error "GPIOPull error: not a valid enum variant"

instance Storable GPIOPull where
    sizeOf :: GPIOPull -> Int
    sizeOf _ = 4

    alignment :: GPIOPull -> Int
    alignment _ = 4

    peek :: Ptr GPIOPull -> IO GPIOPull
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr GPIOPull -> GPIOPull -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- gpio_af_t
data GPIOAF = AF0 | AF1 | AF2 | AF3 | AF4 | AF5 | AF6 | AF7 | AF8 | AF9 | AF10 | AF11 | AF12 | AF13 | AF14 | AF15

instance Enum GPIOAF where
    fromEnum AF0 = 0
    fromEnum AF1 = 1
    fromEnum AF2 = 2
    fromEnum AF3 = 3
    fromEnum AF4 = 4
    fromEnum AF5 = 5
    fromEnum AF6 = 6
    fromEnum AF7 = 7
    fromEnum AF8 = 8
    fromEnum AF9 = 9
    fromEnum AF10 = 10
    fromEnum AF11 = 11
    fromEnum AF12 = 12
    fromEnum AF13 = 13
    fromEnum AF14 = 14
    fromEnum AF15 = 15

    toEnum 0 = AF0
    toEnum 1 = AF1
    toEnum 2 = AF2
    toEnum 3 = AF3
    toEnum 4 = AF4
    toEnum 5 = AF5
    toEnum 6 = AF6
    toEnum 7 = AF7
    toEnum 8 = AF8
    toEnum 9 = AF9
    toEnum 10 = AF10
    toEnum 11 = AF11
    toEnum 12 = AF12
    toEnum 13 = AF13
    toEnum 14 = AF14
    toEnum 15 = AF15
    toEnum _ = error "GPIOAF error: not a valid enum variant"

instance Storable GPIOAF where
    sizeOf :: GPIOAF -> Int
    sizeOf _ = 4

    alignment :: GPIOAF -> Int
    alignment _ = 4

    peek :: Ptr GPIOAF -> IO GPIOAF
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr GPIOAF -> GPIOAF -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- gpio_config_t
data GPIOConfig = GPIOConfig
    { mode :: GPIOMode
    , pull :: GPIOPull
    , alternate :: GPIOAF
    , security_domain :: GPIOSecurity
    }

instance Storable GPIOConfig where
    sizeOf :: GPIOConfig -> Int
    sizeOf _ =
        sizeOf (undefined :: GPIOMode)
            + sizeOf (undefined :: GPIOPull)
            + sizeOf (undefined :: GPIOAF)
            + sizeOf (undefined :: GPIOSecurity)

    alignment :: GPIOConfig -> Int
    alignment _ = 4

    peek :: Ptr GPIOConfig -> IO GPIOConfig
    peek ptr =
        GPIOConfig
            <$> peek (castPtr ptr)
            <*> peek (castPtr ptr `plusPtr` 4)
            <*> peek (castPtr ptr `plusPtr` 8)
            <*> peek (castPtr ptr `plusPtr` 12)

    poke :: Ptr GPIOConfig -> GPIOConfig -> IO ()
    poke ptr GPIOConfig{mode = m, pull = p, alternate = a, security_domain = s} = do
        poke (castPtr ptr) m
        poke (castPtr ptr `plusPtr` 4) p
        poke (castPtr ptr `plusPtr` 8) a
        poke (castPtr ptr `plusPtr` 12) s

-- GPIO port
data GPIOPort = A | B | C | D | E | F | G | H

instance Enum GPIOPort where
    fromEnum A = 0
    fromEnum B = 1
    fromEnum C = 2
    fromEnum D = 3
    fromEnum E = 4
    fromEnum F = 5
    fromEnum G = 6
    fromEnum H = 7

    toEnum 0 = A
    toEnum 1 = B
    toEnum 2 = C
    toEnum 3 = D
    toEnum 4 = E
    toEnum 5 = F
    toEnum 6 = G
    toEnum 7 = H
    toEnum _ = error "GPIOPort error: not a valid enum variant"

instance Storable GPIOPort where
    sizeOf :: GPIOPort -> Int
    sizeOf _ = 4

    alignment :: GPIOPort -> Int
    alignment _ = 4

    peek :: Ptr GPIOPort -> IO GPIOPort
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr GPIOPort -> GPIOPort -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- This is the public API, exported to the user, along with the relevant types declared above

gpio_init :: GPIO -> GPIOConfig -> IO ()
gpio_init gpio_dev_t gpio_cfg = with gpio_cfg $ \ptr -> gpioInit gpio_dev_t (castPtr ptr)

gpio_write :: GPIO -> Bool -> IO ()
gpio_write gpio_dev_t level = gpioWrite gpio_dev_t (toCInt level)

gpio_read :: GPIO -> IO Bool
gpio_read gpio_dev_t = do
    b <- gpioRead gpio_dev_t
    return $ fromCInt b

gpio_toggle :: GPIO -> IO ()
gpio_toggle gpio_dev_t = gpioToggle gpio_dev_t
