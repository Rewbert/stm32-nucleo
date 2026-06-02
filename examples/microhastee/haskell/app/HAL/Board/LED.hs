module HAL.Board.LED (
    LED (..),
    board_led,
    toggle_led,
) where

import HAL.Drivers.GPIO (GPIO, gpio_toggle, gpio_write)
import Foreign.C.Types
import Foreign.HAL.Utils
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "boards/board.h board_led" c_board_led :: CInt -> IO GPIO

data LED = GREEN | BLUE | RED

instance Enum LED where
    fromEnum :: LED -> Int
    fromEnum GREEN = 0
    fromEnum BLUE = 1
    fromEnum RED = 2

    toEnum :: Int -> LED
    toEnum 0 = GREEN
    toEnum 1 = BLUE
    toEnum 2 = RED

instance Storable LED where
    sizeOf :: LED -> Int
    sizeOf _ = 4

    alignment :: LED -> Int
    alignment _ = 4

    peek :: Ptr LED -> IO LED
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr LED -> LED -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- API

board_led :: LED -> IO GPIO
board_led led_dev_t = c_board_led (toCInt led_dev_t)

toggle_led :: LED -> IO ()
toggle_led led_dev_t = board_led led_dev_t >>= gpio_toggle
