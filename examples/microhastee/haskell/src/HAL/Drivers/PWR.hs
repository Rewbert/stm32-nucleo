module HAL.Drivers.PWR (
    PWR,
    PWRRange (..),
    pwr_enable_vddio2,
    pwr_disable_vddio2,
    pwr_set_voltage_scaling,
) where

import Foreign.C.Types (CInt (..))
import Foreign.HAL.Utils
import Foreign.Ptr (Ptr)

foreign import ccall "drivers/pwr.h pwr_enable_vddio2" pwrEnableVddio2 :: PWR -> IO ()
foreign import ccall "drivers/pwr.h pwr_disable_vddio2" pwrDisableVddio2 :: PWR -> IO ()
foreign import ccall "drivers/pwr.h pwr_set_voltage_scaling" pwrSetVoltageScaling :: PWR -> CInt -> IO ()

type PWR = Ptr () -- pwr_dev_t *

data PWRRange
    = PWR_RANGE1
    | PWR_RANGE2
    | PWR_RANGE3
    | PWR_RANGE4

instance Enum PWRRange where
    fromEnum :: PWRRange -> Int
    fromEnum PWR_RANGE1 = 0
    fromEnum PWR_RANGE2 = 1
    fromEnum PWR_RANGE3 = 2
    fromEnum PWR_RANGE4 = 3

    toEnum :: Int -> PWRRange
    toEnum 0 = PWR_RANGE1
    toEnum 1 = PWR_RANGE2
    toEnum 2 = PWR_RANGE3
    toEnum 3 = PWR_RANGE4
    toEnum _ = error "PWRRange error: not a valid enum variant"

pwr_enable_vddio2 :: PWR -> IO ()
pwr_enable_vddio2 pwr_dev_t = pwrEnableVddio2 pwr_dev_t

pwr_disable_vddio2 :: PWR -> IO ()
pwr_disable_vddio2 pwr_dev_t = pwrDisableVddio2 pwr_dev_t

pwr_set_voltage_scaling :: PWR -> PWRRange -> IO ()
pwr_set_voltage_scaling pwr_dev_t range = pwrSetVoltageScaling pwr_dev_t (toCInt range)
