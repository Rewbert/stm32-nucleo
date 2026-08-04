-- | Nicer wrapper around HAL.Drivers.UDB.Internal.hs. arbitrary, 'Hashable' keys
-- and arbitrary, 'NFData' values instead of raw 'Data.Word.Word32' keys and
-- byte lists.
module HAL.Drivers.UDB (
    UDB
    , init_db
    , insert
    , lookup
    , delete
) where

import Prelude hiding (lookup)

import Data.Hashable (Hashable (..))
import Control.DeepSeq (NFData, force)

import Data.Word (Word8, Word32)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import HAL.Drivers.UDB.Internal (
    UDB,
    UDBStatus (..),
    board_udb,
    board_udb_config,
    udb_mount,
    udb_format,
    udb_put,
    udb_get,
    udb_delete,
    )

-- BFILE plumbing (same primitives "Effectful.Internal.Setup"/"Secure"/"NonSecure"
-- use; redeclared locally since HAL.Drivers.* never depends on Effectful.*).

data BFILE

foreign import ccall "openb_wr_mem"  c_openb_wr_mem :: IO (Ptr BFILE)
foreign import ccall "openb_rd_mem"  c_openb_rd_mem :: Ptr Word8 -> Int -> IO (Ptr BFILE)
foreign import ccall "get_mem"       c_get_mem      :: Ptr BFILE -> Ptr (Ptr Word8) -> Ptr Int -> IO ()
foreign import ccall "closeb_rd_mem" c_closeb       :: Ptr BFILE -> IO ()

-- These are primitives offered by the MHS RTS.
primHSerialize   :: Ptr BFILE -> a -> IO ()
primHSerialize    = _primitive "IO.serialize"
primHDeserialize :: Ptr BFILE -> IO a
primHDeserialize  = _primitive "IO.deserialize"

-- API

-- | Mount this domain's database, formatting it on first boot
-- ('UDB_ERR_NO_DB'). Crashes if the database can't be mounted or formatted --
-- there is no sensible value of type 'UDB' to hand back otherwise.
init_db :: IO UDB
init_db = do
    db  <- board_udb
    cfg <- board_udb_config
    st  <- udb_mount db cfg
    st' <- case st of
        UDB_ERR_NO_DB -> udb_format db cfg
        _             -> return st
    case st' of
        UDB_OK -> return db
        _      -> error ("HAL.Drivers.UDB.init_db: could not mount or format the database: " ++ show st')

-- | Insert or update. @val@ is fully forced ('force') before being
-- serialized, so no unevaluated thunk ever ends up written to flash.
insert :: (Hashable key, NFData val) => key -> val -> UDB -> IO ()
insert key val db = do
    wbf   <- c_openb_wr_mem
    primHSerialize wbf (force val)
    bytes <- extractBytes wbf
    c_closeb wbf
    _ <- udb_put db (hashKey key) bytes
    return ()

-- | 'Nothing' if the key has no live entry (or the database returned any
-- other error); 'Just' the deserialized value otherwise. The result type is
-- determined by how you use it, same as 'primHDeserialize' itself -- e.g.
-- @(x :: Maybe Int) <- lookup "foo" db@.
lookup :: Hashable key => key -> UDB -> IO (Maybe a)
lookup key db = do
    let k = hashKey key
    (st0, len0, bytes0) <- udb_get db k 0
    case st0 of
        UDB_OK            -> deserializeMaybe bytes0
        UDB_ERR_TOO_SMALL -> do
            (st1, _, bytes1) <- udb_get db k len0
            case st1 of
                UDB_OK -> deserializeMaybe bytes1
                _      -> return Nothing
        _ -> return Nothing

-- | Idempotent -- see 'HAL.Drivers.UDB.Internal.udb_delete'.
delete :: Hashable key => key -> UDB -> IO ()
delete key db = do
    _ <- udb_delete db (hashKey key)
    return ()

-- Helpers

hashKey :: Hashable key => key -> Word32
hashKey = fromIntegral . hash

-- Extract the serialized bytes from a write-BFILE and free its buffer.
extractBytes :: Ptr BFILE -> IO [Word8]
extractBytes wbf =
    alloca $ \bufPtrPtr ->
    alloca $ \lenPtr -> do
        c_get_mem wbf bufPtrPtr lenPtr
        bufPtr <- peek bufPtrPtr
        len    <- peek lenPtr
        bytes  <- peekBytes bufPtr len
        free bufPtr
        return bytes

-- Wrap a byte buffer as a read-BFILE and deserialize a value from it, all
-- within the buffer's lifetime.
deserializeMaybe :: [Word8] -> IO (Maybe a)
deserializeMaybe bytes =
    allocaBytes (max 1 (length bytes)) $ \ptr -> do
        pokeBytes ptr bytes
        rbf <- c_openb_rd_mem ptr (length bytes)
        val <- primHDeserialize rbf
        c_closeb rbf
        return (Just val)

pokeBytes :: Ptr Word8 -> [Word8] -> IO ()
pokeBytes ptr = go 0
  where
    go _ [] = return ()
    go i (w:ws) = poke (ptr `plusPtr` i) w >> go (i + 1) ws

peekBytes :: Ptr Word8 -> Int -> IO [Word8]
peekBytes ptr len = mapM (\i -> peek (ptr `plusPtr` i)) [0 .. len - 1]
