module ExampleWithPersistent where

import Setup
--import Secure -- import Secure -- import only one of these, and compile the program twice
import Secure

-- low level secure functions, that can only be invoked by the secure application
-- this is enforced by the type checker
-- NOTE: These should be Secure Int. I've emailed Lennart about it.
foreign import ccall "unlock"    unlock_c    :: IO Int
foreign import ccall "lock"      lock_c      :: IO Int
foreign import ccall "verifyPin" verifyPin_c :: Int -> Int -> Int -> Int -> IO Int
foreign import ccall "setPin"    setPin_c    :: Int -> Int -> Int -> Int -> IO Int

-- low level non secure functions, that can be invoked by the non secure applications
foreign import ccall "readKeyPress"        readKeyPress_c        :: IO Int
foreign import ccall "readKeyPressTimeout" readKeyPressTimeout_c :: Int -> IO Int

-- * These three values are codes that can be returned from the C world, to indicate which
-- functionality is requested from the keypad
changePinKey :: Int
changePinKey = 5

lockDoorKey :: Int
lockDoorKey = 6

unlockDoorKey :: Int
unlockDoorKey = 7

data Function = Lock | Unlock | ChangePin

mkFunction :: Int -> Maybe Function
mkFunction k | k == changePinKey  = Just ChangePin
             | k == lockDoorKey   = Just Lock
             | k == unlockDoorKey = Just Unlock
             | otherwise          = Nothing


-- * Functions residing solely in the secure world

verifyCode :: (Int, Int, Int, Int) -> Secure Bool
verifyCode (v1,v2,v3,v4) = do
    p <- unsafeLiftIO $ verifyPin_c v1 v2 v3 v4
    return $ p /= 0

-- * Functions in the secure world that can be called from the non secure world (see app-function)

unlockDoor :: (Int, Int, Int, Int) -> Secure Bool
unlockDoor code = do
    b <- verifyCode code
    if b
        then ((/=) 0) <$> (unsafeLiftIO $ unlock_c)
        else return False

lockDoor :: (Int, Int, Int, Int) -> Secure Bool
lockDoor code = do
    b <- verifyCode code
    if b
        then ((/=) 0) <$> (unsafeLiftIO $ lock_c)
        else return False

changePin :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Secure Bool
changePin old (v1,v2,v3,v4) = do
    b <- verifyCode old
    if b
        then ((/=) 0) <$> (unsafeLiftIO $ setPin_c v1 v2 v3 v4)
        else return False

-- * Functions in the non secure world

-- | non secure operation that reads a sequence of 4 numbers from the keypad
-- returns either Just the pin, or Nothing if more than 5 seconds elapse without
-- any key being pressed
readPin :: IO (Maybe (Int, Int, Int, Int))
readPin = go []
  where
    go :: [Int] -> IO (Maybe (Int, Int, Int, Int))
    go (x:y:z:w:_) = return $ Just (x,y,z,w)
    go xs = do
        k <- readKeyPressTimeout_c 10000
        case k of
            -1 -> return Nothing
            _ -> go (xs ++ [k])

-- | Helper type to manage the API of callable functions in the secure world
data NSCApi = NSCApi { changeThePin  :: Callable ((Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Secure Bool)
                     , unlockTheDoor :: Callable ((Int, Int, Int, Int) -> Secure Bool)
                     , lockTheDoor   :: Callable ((Int, Int, Int, Int) -> Secure Bool)
                     }

reportResult :: Bool -> IO ()
reportResult True  = putStrLn "Operation successful"
reportResult False = putStrLn "Operation failed"

-- | The function that executes commands in the non secure world, potentially invoking non secure callable
-- functions in the secure world
handleFunction :: NSCApi -> Function -> IO ()
handleFunction api Lock = do
    pin <- readPin
    case pin of
        Just pin' -> do
            res <- sg $ lockTheDoor api <.> pin'
            reportResult res
            keypad api
        Nothing -> keypad api
handleFunction api Unlock = do
    pin <- readPin
    case pin of
        Just pin' -> do
            res <- sg $ unlockTheDoor api <.> pin'
            reportResult res
            keypad api
        Nothing -> keypad api
handleFunction api ChangePin = do
    old <- readPin
    case old of
        Just old' -> do
            new <- readPin
            case new of
                Just new' -> do
                    res <- sg $ changeThePin api <.> old' <.> new'
                    reportResult res
                    keypad api
                Nothing -> keypad api
        Nothing -> keypad api

-- | The keypad function will read a command from the keypad and handle them
keypad :: NSCApi -> IO ()
keypad api = do
    f <- mkFunction <$> readKeyPress_c
    maybe (keypad api) (handleFunction api) f

-- | The application, where we allocate the initial secure state, mark which functions are
-- non secure callable, and eventually launch the non secure application
app :: Setup ()
app = do    
    ctp <- callable $ changePin
    ltd <- callable $ lockDoor  
    utd <- callable $ unlockDoor
    
    nonSecure $ keypad $ NSCApi ctp utd ltd

main :: IO ()
main = runSetup app