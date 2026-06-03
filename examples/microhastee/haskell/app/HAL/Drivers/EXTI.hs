module HAL.Drivers.EXTI (
    EXTI,
    EXTIEdge (..),
    EXTISecurity (..),
    EXTIConfig (..),
    exti_dev_t_size,
    exti_backend_t_size,
    exti_init,
    exti_set_security,
    exti_irqn,
    exti_enable,
    exti_disable,
) where

import Data.Word

import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import HAL.Drivers.GPIO

foreign import ccall "drivers/exti.h exti_init" c_exti_init :: EXTI -> Ptr () -> IO ()
foreign import ccall "drivers/exti.h exti_register_callback" c_exti_register_callback :: EXTI -> Ptr () -> IO () -- actually, think more about this, and don't implement this now
foreign import ccall "drivers/exti.h exti_set_security" c_exti_set_security :: EXTI -> CInt -> IO ()
foreign import ccall "drivers/exti.h exti_irqn" c_exti_irqn :: EXTI -> IO CInt
foreign import ccall "drivers/exti.h exti_enable" c_exti_enable :: EXTI -> IO ()
foreign import ccall "drivers/exti.h exti_disable" c_exti_disable :: EXTI -> IO ()

type EXTI = Ptr () -- exti_dev_t *

data EXTIEdge
    = RISING
    | FALLING
    | BOTH

instance Enum EXTIEdge where
    fromEnum RISING = 0
    fromEnum FALLING = 1
    fromEnum BOTH = 2

    toEnum 0 = RISING
    toEnum 1 = FALLING
    toEnum 2 = BOTH

instance Storable EXTIEdge where
    sizeOf :: EXTIEdge -> Int
    sizeOf _ = 4

    alignment :: EXTIEdge -> Int
    alignment _ = 4

    peek :: Ptr EXTIEdge -> IO EXTIEdge
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr EXTIEdge -> EXTIEdge -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

data EXTISecurity = EXTISecure | EXTINonsecure

instance Enum EXTISecurity where
    fromEnum EXTISecure = 0
    fromEnum EXTINonsecure = 1

    toEnum 0 = EXTISecure
    toEnum 1 = EXTINonsecure
    toEnum _ = error "EXTISecurity error: not a valid enum variant"

instance Storable EXTISecurity where
    sizeOf :: EXTISecurity -> Int
    sizeOf _ = 4

    alignment :: EXTISecurity -> Int
    alignment _ = 4

    peek :: Ptr EXTISecurity -> IO EXTISecurity
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr EXTISecurity -> EXTISecurity -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

data EXTIConfig = EXTIConfig
    { port :: GPIOPort -- 4
    , pin :: Int -- 1
    , edge :: EXTIEdge -- 4
    }

instance Storable EXTIConfig where
    sizeOf :: EXTIConfig -> Int
    sizeOf _ = 12

    alignment :: EXTIConfig -> Int
    alignment _ = 4

    peek :: Ptr EXTIConfig -> IO EXTIConfig
    peek ptr =
        EXTIConfig
            <$> peek (castPtr ptr)
            <*> (fromIntegral <$> (peek (castPtr ptr `plusPtr` 4) :: IO Word8))
            <*> peek (castPtr ptr `plusPtr` 8)

    poke :: Ptr EXTIConfig -> EXTIConfig -> IO ()
    poke
        ptr
        EXTIConfig
            { port = po
            , pin = pi
            , edge = e
            } = do
            poke (castPtr ptr) po
            poke (castPtr ptr `plusPtr` 4) (fromIntegral pi :: Word8)
            poke (castPtr ptr `plusPtr` 8) e

exti_dev_t_size :: Int
exti_dev_t_size = 16

exti_backend_t_size :: Int
exti_backend_t_size = 4 -- one uint8_t, but padding changes it?

-- API

exti_init :: EXTI -> EXTIConfig -> IO ()
exti_init exti_dev_t exti_cfg = do
    with exti_cfg $ \ptr -> do
        c_exti_init exti_dev_t (castPtr ptr)

exti_set_security :: EXTI -> EXTISecurity -> IO ()
exti_set_security exti_dev_t security =
    c_exti_set_security exti_dev_t (CInt $ fromInteger $ toInteger $ fromEnum security)

exti_irqn :: EXTI -> IO Int
exti_irqn exti_dev_t = fromIntegral <$> c_exti_irqn exti_dev_t

exti_enable :: EXTI -> IO ()
exti_enable exti_dev_t = c_exti_enable exti_dev_t

exti_disable :: EXTI -> IO ()
exti_disable exti_dev_t = c_exti_disable exti_dev_t
