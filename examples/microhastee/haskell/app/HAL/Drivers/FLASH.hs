module HAL.Drivers.FLASH (
    FLASH,
    flash_set_latency,
    flash_get_latency,
) where

import Data.Word (Word32)
import Foreign.Ptr (Ptr)

type FLASH = Ptr () -- flash_dev_t *

foreign import ccall "drivers/flash.h flash_set_latency" flashSetLatency :: FLASH -> Word32 -> IO ()
foreign import ccall "drivers/flash.h flash_get_latency" flashGetLatency :: FLASH -> IO Word32

flash_set_latency :: FLASH -> Int -> IO ()
flash_set_latency flash_dev_t wait_states = flashSetLatency flash_dev_t (fromInteger $ toInteger wait_states)

flash_get_latency :: FLASH -> IO Int
flash_get_latency flash_dev_t = do
    ui <- flashGetLatency flash_dev_t
    return $ fromInteger $ toInteger ui
