module Drivers.RCC where

import Foreign.HAL.Utils
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Data.Word (Word32)

import Drivers.PWR
import Drivers.FLASH

foreign import ccall "drivers/rcc.h rcc_enable" rccEnable :: RCC -> CInt -> IO ()
foreign import ccall "drivers/rcc.h rcc_disable" rccDisable :: RCC -> CInt -> IO ()
foreign import ccall "drivers/rcc.h rcc_is_enabled" rccIsEnabled :: RCC -> CInt -> IO CInt
foreign import ccall "drivers/rcc.h rcc_set_peripheral_clock" rccSetPeripheralClock :: RCC -> CInt -> CInt -> IO ()
foreign import ccall "drivers/rcc.h rcc_configure_pll" rccConfigurePll :: RCC -> PWR -> FLASH -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

type RCC    = Ptr ()  -- rcc_dev_t *

-- rcc_periph_t
data RCCPeriph
    = RCC_GPIOA
    | RCC_GPIOB
    | RCC_GPIOC
    | RCC_GPIOD
    | RCC_GPIOE
    | RCC_GPIOF
    | RCC_GPIOG
    | RCC_GPIOH
    | RCC_LPUART1
    | RCC_PWR
    | RCC_GTZC
    | RCC_GTZC2
    | RCC_USART1

instance Enum RCCPeriph where
    fromEnum RCC_GPIOA   = 0
    fromEnum RCC_GPIOB   = 1
    fromEnum RCC_GPIOC   = 2
    fromEnum RCC_GPIOD   = 3
    fromEnum RCC_GPIOE   = 4
    fromEnum RCC_GPIOF   = 5
    fromEnum RCC_GPIOG   = 6
    fromEnum RCC_GPIOH   = 7
    fromEnum RCC_LPUART1 = 8
    fromEnum RCC_PWR     = 9
    fromEnum RCC_GTZC    = 10
    fromEnum RCC_GTZC2   = 11
    fromEnum RCC_USART1  = 12

    toEnum 0 = RCC_GPIOA
    toEnum 1 = RCC_GPIOB
    toEnum 2 = RCC_GPIOC
    toEnum 3 = RCC_GPIOD
    toEnum 4 = RCC_GPIOE
    toEnum 5 = RCC_GPIOF
    toEnum 6 = RCC_GPIOG
    toEnum 7 = RCC_GPIOH
    toEnum 8 = RCC_LPUART1
    toEnum 9 = RCC_PWR
    toEnum 10 = RCC_GTZC
    toEnum 11 = RCC_GTZC2
    toEnum 12 = RCC_USART1
    toEnum _ = error "RCCPeriph error: not a valid enum variant"

instance Storable RCCPeriph where
    sizeOf :: RCCPeriph -> Int
    sizeOf _ = 4

    alignment :: RCCPeriph -> Int
    alignment _ = 4

    peek :: Ptr RCCPeriph -> IO RCCPeriph
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr RCCPeriph -> RCCPeriph -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

-- rcc_periph_clock_source_t
data RCCPeriphClockSource
    = RCC_PCKL1
    | RCC_PCKL2
    | RCC_PCKL3
    | RCC_SYSCLK
    | RCC_HSI16
    | RCC_HSI48
    | RCC_LSE
    | RCC_HSE
    | RCC_MSI
    | RCC_PLL_Q
    | RCC_PLL_SAI1_P
    | RCC_PLL_SAI1_R
    | RCC_NO_CLOCK

instance Enum RCCPeriphClockSource where
    fromEnum RCC_PCKL1      = 0
    fromEnum RCC_PCKL2      = 1
    fromEnum RCC_PCKL3      = 2
    fromEnum RCC_SYSCLK     = 3
    fromEnum RCC_HSI16      = 4
    fromEnum RCC_HSI48      = 5
    fromEnum RCC_LSE        = 6
    fromEnum RCC_HSE        = 7
    fromEnum RCC_MSI        = 8
    fromEnum RCC_PLL_Q      = 9
    fromEnum RCC_PLL_SAI1_P = 10
    fromEnum RCC_PLL_SAI1_R = 11
    fromEnum RCC_NO_CLOCK   = 12

    toEnum 0  = RCC_PCKL1
    toEnum 1  = RCC_PCKL2
    toEnum 2  = RCC_PCKL3
    toEnum 3  = RCC_SYSCLK
    toEnum 4  = RCC_HSI16
    toEnum 5  = RCC_HSI48
    toEnum 6  = RCC_LSE
    toEnum 7  = RCC_HSE
    toEnum 8  = RCC_MSI
    toEnum 9  = RCC_PLL_Q
    toEnum 10 = RCC_PLL_SAI1_P
    toEnum 11 = RCC_PLL_SAI1_R
    toEnum 12 = RCC_NO_CLOCK
    toEnum _  = error "RCCPeriphClockSource error: not valid enum variant"

instance Storable RCCPeriphClockSource where
    sizeOf :: RCCPeriphClockSource -> Int
    sizeOf _ = 4

    alignment :: RCCPeriphClockSource -> Int
    alignment _ = 4

    peek :: Ptr RCCPeriphClockSource -> IO RCCPeriphClockSource
    peek ptr = (toEnum . fromInteger . (toInteger :: CInt -> Integer)) <$> peek (castPtr ptr)

    poke :: Ptr RCCPeriphClockSource -> RCCPeriphClockSource -> IO ()
    poke ptr v = poke (castPtr ptr) (CInt $ fromInteger $ toInteger $ fromEnum v)

rcc_enable :: RCC -> RCCPeriph -> IO ()
rcc_enable rcc_dev_t periph = rccEnable rcc_dev_t (toCInt periph)

rcc_disable :: RCC -> RCCPeriph -> IO ()
rcc_disable rcc_dev_t periph = rccDisable rcc_dev_t (toCInt periph)

rcc_is_enabled :: RCC -> RCCPeriph -> IO Bool
rcc_is_enabled rcc_dev_t periph = do
    ci <- rccIsEnabled rcc_dev_t (toCInt periph)
    return $ fromCInt ci

rcc_set_peripheral_clock :: RCC -> RCCPeriph -> RCCPeriphClockSource -> IO ()
rcc_set_peripheral_clock rcc_dev_t periph clock_source = rccSetPeripheralClock rcc_dev_t (toCInt periph) (toCInt clock_source)

rcc_configure_pll :: RCC -> PWR -> FLASH -> Int -> Int -> Int -> Int -> IO ()
rcc_configure_pll rcc_dev_t pwr_dev_t flash_dev_t pll_n pll_m pll_div target_sysclk_hz =
    rccConfigurePll rcc_dev_t pwr_dev_t flash_dev_t (toCUInt pll_n) (toCUInt pll_m) (toCUInt pll_div) (toCUInt target_sysclk_hz)