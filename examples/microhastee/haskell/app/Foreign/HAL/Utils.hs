module Foreign.HAL.Utils (toCInt, fromCInt, toCUInt, toCUChar) where

import Foreign.C.Types

toCInt :: (Enum a) => a -> CInt
toCInt a = CInt $ fromEnum a

toCUInt :: (Enum a) => a -> CUInt
toCUInt a = CUInt $ fromInteger $ toInteger $ fromEnum a

fromCInt :: (Enum a) => CInt -> a
fromCInt (CInt i) = toEnum i

toCUChar :: Int -> CUChar
toCUChar i = CUChar $ (fromInteger $ toInteger i)
