module HAL.Board.Board (
    board_init,
    board_configure_pll,
    board_sysclk_hz,
    board_gpio_create,
    board_exti_create,
    board_rcc,
    board_pwr,
    board_flash,
    board_console,
    board_console_periph,
    board_tzsc,
) where

import Foreign.C.Types
import Foreign.HAL.Utils
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Data.Word (Word32)
import HAL.Drivers.EXTI
import HAL.Drivers.FLASH
import HAL.Drivers.GPIO
import HAL.Drivers.PWR
import HAL.Drivers.RCC
import HAL.Drivers.TZSC
import HAL.Drivers.UART

foreign import ccall "boards/board.h board_init" boardInit :: IO ()
foreign import ccall "boards/board.h board_configure_pll" boardConfigurePll :: IO ()

foreign import ccall "boards/board.h board_sysclk_hz" boardSysclkHz :: IO Word32
foreign import ccall "boards/board.h board_console" boardConsole :: IO UART
foreign import ccall "boards/board.h board_console_periph" boardConsolePeriph :: IO CInt
foreign import ccall "boards/board.h board_rcc" boardRcc :: IO RCC
foreign import ccall "boards/board.h board_flash" boardFlash :: IO FLASH
foreign import ccall "boards/board.h board_pwr" boardPwr :: IO PWR
foreign import ccall "boards/board.h board_tzsc" boardTzsc :: IO TZSC

foreign import ccall "boards/board.h board_gpio_create" c_board_gpio_create :: GPIO -> CInt -> CUChar -> Ptr () -> IO ()
foreign import ccall "boards/board.h board_exti_create" c_board_exti_create :: EXTI -> Ptr () -> CUChar -> IO ()

-- * Board Initialisation

board_init :: IO ()
board_init = boardInit

board_configure_pll :: IO ()
board_configure_pll = boardConfigurePll

board_sysclk_hz :: IO Int
board_sysclk_hz = (fromInteger . toInteger) <$> boardSysclkHz

-- * GPIO creation

-- maybe move these two to Drivers.GPIO?
gpio_dev_t_size :: Int
gpio_dev_t_size = 16

gpio_backend_t_size :: Int
gpio_backend_t_size = 6

board_gpio_create :: GPIOPort -> Int -> IO GPIO
board_gpio_create port pin = do
    gpio_dev_t <- mallocBytes gpio_dev_t_size :: IO GPIO
    gpio_backend_t <- mallocBytes gpio_backend_t_size :: IO (Ptr ())

    c_board_gpio_create gpio_dev_t (toCInt port) (toCUChar pin) gpio_backend_t
    return gpio_dev_t

-- * EXTI creation

board_exti_create :: Int -> IO EXTI
board_exti_create pin = do
    exti_dev_t <- mallocBytes exti_dev_t_size :: IO EXTI
    exti_backend_t <- mallocBytes exti_backend_t_size :: IO (Ptr ())

    c_board_exti_create exti_dev_t exti_backend_t (toCUChar pin)
    return exti_dev_t

-- * Devices

board_rcc :: IO RCC
board_rcc = boardRcc

board_pwr :: IO PWR
board_pwr = boardPwr

board_flash :: IO FLASH
board_flash = boardFlash

board_console :: IO UART
board_console = boardConsole

board_console_periph :: IO TZSCPeriph
board_console_periph = (toEnum . fromCInt) <$> boardConsolePeriph

board_tzsc :: IO TZSC
board_tzsc = boardTzsc
