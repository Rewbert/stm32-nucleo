module Drivers.UART (
    UART,
    UARTParity (..),
    UARTConfig (..),
    uart_init,
    uart_write,
    uart_read,
) where

import Data.Word (Word32, Word8)
import Foreign.C.String
import Foreign.C.Types
import Foreign.HAL.Utils
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

type UART = Ptr () -- uart_dev_t *

-- uart_config_t passed as Ptr () (caller fills in C)
foreign import ccall "drivers/uart.h uart_init" c_uart_init :: UART -> Ptr () -> IO ()
foreign import ccall "drivers/uart.h uart_write" c_uart_write :: UART -> Ptr CChar -> CInt -> IO ()
foreign import ccall "drivers/uart.h uart_read" c_uart_read :: UART -> Ptr CChar -> CInt -> IO CInt

data UARTParity = NONE | EVEN | ODD

instance Enum UARTParity where
    fromEnum NONE = 0
    fromEnum EVEN = 1
    fromEnum ODD = 2

    toEnum 0 = NONE
    toEnum 1 = EVEN
    toEnum 2 = ODD
    toEnum _ = error "UARTParity error: not a valid enum variant"

instance Storable UARTParity where
    sizeOf :: UARTParity -> Int
    sizeOf _ = 4

    alignment :: UARTParity -> Int
    alignment _ = 4

    peek :: Ptr UARTParity -> IO UARTParity
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr UARTParity -> UARTParity -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

data UARTConfig = UARTConfig
    { baudrate :: Int
    , word_length :: Int
    , stop_bits :: Int
    , parity :: UARTParity
    }

instance Storable UARTConfig where
    sizeOf :: UARTConfig -> Int
    sizeOf _ = 12

    alignment :: UARTConfig -> Int
    alignment _ = 4

    peek :: Ptr UARTConfig -> IO UARTConfig
    peek ptr =
        UARTConfig
            <$> (fromIntegral <$> (peek (castPtr ptr) :: IO Word32))
            <*> (fromIntegral <$> (peek (castPtr ptr `plusPtr` 4) :: IO Word8))
            <*> (fromIntegral <$> (peek (castPtr ptr `plusPtr` 5) :: IO Word8))
            <*> peek (castPtr ptr `plusPtr` 8)

    poke :: Ptr UARTConfig -> UARTConfig -> IO ()
    poke ptr v = do
        poke (castPtr ptr) (fromIntegral (baudrate v) :: Word32)
        poke (castPtr ptr `plusPtr` 4) (fromIntegral (word_length v) :: Word8)
        poke (castPtr ptr `plusPtr` 5) (fromIntegral (stop_bits v) :: Word8)
        poke (castPtr ptr `plusPtr` 8) (parity v)

-- API

uart_init :: UART -> UARTConfig -> IO ()
uart_init uart_dev_t uart_cfg = do
    with uart_cfg $ \ptr -> c_uart_init uart_dev_t (castPtr ptr)

uart_write :: UART -> String -> IO ()
uart_write uart_dev_t str =
    withCAStringLen str $ \(ptr_str, l) -> c_uart_write uart_dev_t ptr_str (toCInt l)

read_buf_size :: Int
read_buf_size = 1024

uart_read :: UART -> IO String
uart_read uart_dev_t = do
    allocaBytes read_buf_size $ \ptr -> do
        l <- c_uart_read uart_dev_t ptr (toCInt read_buf_size)
        peekCAStringLen (ptr, fromCInt l)
