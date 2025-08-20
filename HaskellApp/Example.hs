module Example where

import Setup
import Secure -- import Secure -- import only one of these, and compile the program twice

-- low level secure functions, that can only be invoked by the secure application
-- this is enforced by the type checker
foreign import ccall "unlock" unlock_c :: Secure Int
foreign import ccall "lock"   lock_c   :: Secure Int

-- low level non secure functions, that can be invoked by the non secure applications
foreign import ccall "readKeyPress"        readKeyPress_c        :: IO Int
foreign import ccall "readKeyPressTimeout" readKeyPressTimeout_c :: Int -> IO Int

-- * These three values are codes that can be returned from the C world, to indicate which
-- functionality is requested from the keypad
changePinKey :: Int
changePinKey = -1

lockDoorKey :: Int
lockDoorKey = -2

unlockDoorKey :: Int
unlockDoorKey = -3

data Function = Lock | Unlock | ChangePin

mkFunction :: Int -> Maybe Function
mkFunction k | k == changePinKey  = Just ChangePin
             | k == lockDoorKey   = Just Lock
             | k == unlockDoorKey = Just Unlock
             | otherwise          = Nothing

-- * The state of the secure applications consists of the current state of the door, as well as
-- the secure pin

data SecureState = SecureState { thepincode :: (Int, Int, Int, Int), doorState :: Bool }

-- * Functions residing solely in the secure world

verifyCode :: Secure (SRef SecureState) -> (Int, Int, Int, Int) -> Secure Bool
verifyCode sr candidateCode = do
    r <- sr
    p <- thepincode <$> readSRef r
    return $ p == candidateCode

unlock :: Secure (SRef SecureState) -> Secure ()
unlock sr = do
    r <- sr
    res <- unlock_c
    if res /= 0 then modifySRef r $ \st -> st { doorState = True } else return ()

lock :: Secure (SRef SecureState) -> Secure ()
lock sr = do
    r <- sr
    res <- lock_c
    if res /= 0 then modifySRef r $ \st -> st { doorState = False } else return ()

-- * Functions in the secure world that can be called from the non secure world (see app-function)

-- | Changes the pin code
changePin :: Secure (SRef SecureState) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Secure Bool
changePin sr old new = do
    b <- verifyCode sr old
    if b
        then do
            r <- sr
            modifySRef r $ \st -> st { thepincode = new }
            return True
        else return False

unlockDoor :: Secure (SRef SecureState) -> (Int, Int, Int, Int) -> Secure Bool
unlockDoor sr code = do
    b <- verifyCode sr code
    if b
        then unlock sr >> return True
        else return False

lockDoor :: Secure (SRef SecureState) -> (Int, Int, Int, Int) -> Secure Bool
lockDoor sr code = do
    b <- verifyCode sr code
    if b
        then lock sr >> return True
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
        k <- readKeyPressTimeout_c 5000
        case k of
            -1 -> return Nothing
            _ -> go (xs ++ [k])

-- | Helper type to manage the API of callable functions in the secure world
data NSCApi = NSCApi { changeThePin  :: Callable ((Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Secure Bool)
                     , unlockTheDoor :: Callable ((Int, Int, Int, Int) -> Secure Bool)
                     , lockTheDoor   :: Callable ((Int, Int, Int, Int) -> Secure Bool)
                     }

-- | The function that executes commands in the non secure world, potentially invoking non secure callable
-- functions in the secure world
handleFunction :: NSCApi -> Function -> IO ()
handleFunction api Lock = do
    pin <- readPin
    case pin of
        Just pin' -> do
            res <- sg $ lockTheDoor api <.> pin'
            keypad api
        Nothing -> keypad api
handleFunction api Unlock = do
    pin <- readPin
    case pin of
        Just pin' -> do
            res <- sg $ unlockTheDoor api <.> pin'
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
    securestate <- initialSRef $ SecureState (1,2,3,4) False
    
    ctp <- callable $ changePin  securestate
    ltd <- callable $ lockDoor   securestate
    utd <- callable $ unlockDoor securestate
    
    nonSecure $ keypad $ NSCApi ctp utd ltd

main :: IO ()
main = runSetup app