module Drivers.TZSC (
      TZSC
    , TZSCSecurity(..)
    , TZSCPeriph(..)
    
    , tzsc_set_periph
    , tzsc_lock
) where

import Foreign.HAL.Utils
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "drivers/tzsc.h tzsc_set_periph" c_tzsc_set_periph :: TZSC -> CInt -> CInt -> IO ()
foreign import ccall "drivers/tzsc.h tzsc_lock"       c_tzsc_lock       :: TZSC -> IO ()

type TZSC = Ptr ()  -- tzsc_dev_t *

-- tzsc_security_t
data TZSCSecurity = TZSC_SECURE | TZSC_NONSECURE

instance Enum TZSCSecurity where
    fromEnum :: TZSCSecurity -> Int
    fromEnum TZSC_SECURE = 0
    fromEnum TZSC_NONSECURE = 1

    toEnum :: Int -> TZSCSecurity
    toEnum 0 = TZSC_SECURE
    toEnum 1 = TZSC_NONSECURE
    toEnum _ = error "TZSCSecurity error: not a valid enum variant"

instance Storable TZSCSecurity where
    sizeOf :: TZSCSecurity -> Int
    sizeOf _ = 4

    alignment :: TZSCSecurity -> Int
    alignment _ = 4

    peek :: Ptr TZSCSecurity -> IO TZSCSecurity
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr TZSCSecurity -> TZSCSecurity -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- tzsc_periph_t
data TZSCPeriph = TZSC_LPUART1 | TZSC_USART1 -- there are numerous more, but I just need these two

instance Enum TZSCPeriph where
    fromEnum :: TZSCPeriph -> Int
    fromEnum TZSC_LPUART1 = 21
    fromEnum TZSC_USART1  = 33

    toEnum :: Int -> TZSCPeriph
    toEnum 21 = TZSC_LPUART1
    toEnum 33 = TZSC_USART1
    toEnum _ = error "TZSCPeriph error: not a valid enum variant"

instance Storable TZSCPeriph where
    sizeOf :: TZSCPeriph -> Int
    sizeOf _ = 4

    alignment :: TZSCPeriph -> Int
    alignment _ = 4

    peek :: Ptr TZSCPeriph -> IO TZSCPeriph
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr TZSCPeriph -> TZSCPeriph -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- API

tzsc_set_periph :: TZSC -> TZSCPeriph -> TZSCSecurity -> IO ()
tzsc_set_periph tzsc_dev_t periph sec = c_tzsc_set_periph tzsc_dev_t (toCInt periph) (toCInt sec)

tzsc_lock :: TZSC -> IO ()
tzsc_lock tzsc_dev_t = c_tzsc_lock tzsc_dev_t