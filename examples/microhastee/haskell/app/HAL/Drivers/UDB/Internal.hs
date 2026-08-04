module HAL.Drivers.UDB.Internal (
    UDB,
    UDBConfig,
    UDBStatus (..),
    board_udb,
    board_udb_config,
    udb_mount,
    udb_format,
    udb_put,
    udb_get,
    udb_delete,
) where

import Data.Word (Word8, Word32)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

type UDB       = Ptr ()  -- udb_t *
type UDBConfig = Ptr ()  -- const udb_config_t *

foreign import ccall "udb-drv.h board_udb"        c_board_udb        :: IO UDB
foreign import ccall "udb-drv.h board_udb_config" c_board_udb_config :: IO UDBConfig

foreign import ccall "udb.h udb_mount"  c_udb_mount  :: UDB -> UDBConfig -> IO CInt
foreign import ccall "udb.h udb_format" c_udb_format :: UDB -> UDBConfig -> IO CInt
foreign import ccall "udb.h udb_put"    c_udb_put    :: UDB -> Word32 -> Ptr Word8 -> Word32 -> IO CInt
foreign import ccall "udb.h udb_get"    c_udb_get    :: UDB -> Word32 -> Ptr Word8 -> Word32 -> Ptr Word32 -> IO CInt
foreign import ccall "udb.h udb_delete" c_udb_delete :: UDB -> Word32 -> IO CInt

data UDBStatus
    = UDB_OK
    | UDB_ERR_NOT_FOUND
    | UDB_ERR_FULL
    | UDB_ERR_TOO_SMALL
    | UDB_ERR_TOO_LARGE
    | UDB_ERR_IO
    | UDB_ERR_CORRUPT
    | UDB_ERR_NO_DB
    | UDB_ERR_UNSUPPORTED
    | UDB_ERR_INVAL
    deriving (Eq, Show)

instance Enum UDBStatus where
    fromEnum UDB_OK              = 0
    fromEnum UDB_ERR_NOT_FOUND   = 1
    fromEnum UDB_ERR_FULL        = 2
    fromEnum UDB_ERR_TOO_SMALL   = 3
    fromEnum UDB_ERR_TOO_LARGE   = 4
    fromEnum UDB_ERR_IO          = 5
    fromEnum UDB_ERR_CORRUPT     = 6
    fromEnum UDB_ERR_NO_DB       = 7
    fromEnum UDB_ERR_UNSUPPORTED = 8
    fromEnum UDB_ERR_INVAL       = 9

    toEnum 0 = UDB_OK
    toEnum 1 = UDB_ERR_NOT_FOUND
    toEnum 2 = UDB_ERR_FULL
    toEnum 3 = UDB_ERR_TOO_SMALL
    toEnum 4 = UDB_ERR_TOO_LARGE
    toEnum 5 = UDB_ERR_IO
    toEnum 6 = UDB_ERR_CORRUPT
    toEnum 7 = UDB_ERR_NO_DB
    toEnum 8 = UDB_ERR_UNSUPPORTED
    toEnum 9 = UDB_ERR_INVAL
    toEnum _ = error "UDBStatus error: not a valid enum variant"

-- API

-- | Each domain (secure/nonsecure) has its own database, backed by the last
-- two erase pages of that domain's flash bank (examples/microhastee/shared/udb-drv.c).
board_udb :: IO UDB
board_udb = c_board_udb

board_udb_config :: IO UDBConfig
board_udb_config = c_board_udb_config

-- | Validate the segment headers, pick the current segment, locate the
-- append cursor. Never programs or erases anything. On 'UDB_ERR_NO_DB' the
-- database has never been formatted -- call 'udb_format' explicitly.
udb_mount :: UDB -> UDBConfig -> IO UDBStatus
udb_mount db cfg = toCode <$> c_udb_mount db cfg

-- | Destroys all existing content. Never call implicitly -- only after
-- 'udb_mount' returns 'UDB_ERR_NO_DB'.
udb_format :: UDB -> UDBConfig -> IO UDBStatus
udb_format db cfg = toCode <$> c_udb_format db cfg

-- | Insert or update; an empty value list is a valid presence-flag entry.
udb_put :: UDB -> Word32 -> [Word8] -> IO UDBStatus
udb_put db key val =
    allocaBytes (max 1 len) $ \ptr -> do
        pokeBytes ptr val
        toCode <$> c_udb_put db key ptr (fromIntegral len)
  where
    len = length val

-- | Copies the value for @key@ into a freshly allocated buffer of the given
-- capacity. Returns (status, actual-or-required length, bytes); bytes is
-- only meaningful on 'UDB_OK'. On 'UDB_ERR_TOO_SMALL' the returned length
-- tells you how big a retry buffer needs to be -- this does not retry for
-- you.
udb_get :: UDB -> Word32 -> Int -> IO (UDBStatus, Int, [Word8])
udb_get db key cap =
    allocaBytes (max 1 cap) $ \buf ->
    alloca $ \lenPtr -> do
        st  <- toCode <$> c_udb_get db key buf (fromIntegral cap) lenPtr
        len <- fromIntegral <$> peek lenPtr
        val <- if st == UDB_OK then peekBytes buf len else return []
        return (st, len, val)

-- | Tombstones the key's live entry. Idempotent -- returns 'UDB_OK' whether
-- or not the key had a live entry (there is no "not found" status for
-- delete).
udb_delete :: UDB -> Word32 -> IO UDBStatus
udb_delete db key = toCode <$> c_udb_delete db key

-- Marshaling helpers (plusPtr + peek/poke, matching the byte-offset style
-- already used by UARTConfig's Storable instance in HAL.Drivers.UART).

pokeBytes :: Ptr Word8 -> [Word8] -> IO ()
pokeBytes ptr = go 0
  where
    go _ [] = return ()
    go i (w:ws) = poke (ptr `plusPtr` i) w >> go (i + 1) ws

peekBytes :: Ptr Word8 -> Int -> IO [Word8]
peekBytes ptr len = mapM (\i -> peek (ptr `plusPtr` i)) [0 .. len - 1]

-- helpers

toCode :: Integral a => a -> UDBStatus
toCode a = toEnum $ fromIntegral a