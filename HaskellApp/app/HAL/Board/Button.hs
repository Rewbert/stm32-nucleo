module HAL.Board.Button (
    Button (..),
    board_button,
    board_button_exti,
) where

import HAL.Drivers.EXTI (EXTI)
import HAL.Drivers.GPIO (GPIO)
import Foreign.C.Types (CInt (..))
import Foreign.HAL.Utils
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "boards/board.h board_button" c_board_button :: CInt -> IO GPIO
foreign import ccall "boards/board.h board_button_exti" c_board_button_exti :: CInt -> IO EXTI

data Button = BLUE_BUTTON

instance Enum Button where
    fromEnum :: Button -> Int
    fromEnum BLUE_BUTTON = 0

    toEnum :: Int -> Button
    toEnum 0 = BLUE_BUTTON
    toEnum _ = error "Button error: not a valid enum variant"

instance Storable Button where
    sizeOf :: Button -> Int
    sizeOf _ = 4

    alignment :: Button -> Int
    alignment _ = 4

    peek :: Ptr Button -> IO Button
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr Button -> Button -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- API

board_button :: Button -> IO GPIO
board_button b = c_board_button (toCInt b)

board_button_exti :: Button -> IO EXTI
board_button_exti b = c_board_button_exti (toCInt b)
