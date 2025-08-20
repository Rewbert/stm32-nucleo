module ExampleNotes where

{-@

-- | This is executed by both the secure and non secure world
data Setup a
newSRef :: a -> Setup (Secure (SRef a))
mkCallable :: a -> Setup (Callable a)

data Secure -- newtype Secure a = Secure (IO a), but we don't expose an MonadIO instance
data SecureRef
readSRef   :: Setup (SRef a) -> Secure a
writeSRef  :: Setup (SRef a) -> a -> Secure ()
modifySRef :: Setup (SRef a) -> (a -> a) -> Secure ()

data Callable a
(<@>)      :: SomeConstraint a => Callable (a -> b) -> Callable b
gateway    :: Value v => Callable (Secure v) -> IO v












-- Secure mutable data
data SecureState

-- completely secure operations, not callable from outside
verifyCode    :: Setup (SRef SecureState) -> (Int, Int, Int, Int) -> Secure Bool
unlock        :: Setup (SRef SecureState) -> Secure ()
lock          :: Setup (SRef SecureState) -> Secure ()

-- secure functions callable from outside
changePin  :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Secure Bool
unlockDoor :: (Int, Int, Int, Int) -> Secure Bool
lockDoor   :: (Int, Int, Int, Int) -> Secure Bool

-- non-secure operations
readKeypress        :: IO Int
readKeyPressTimeout :: Int -> IO (Maybe Int)
readPin             :: IO (Int, Int, Int, Int)

-- app

changePinKey :: Int
changePinKey = -1

lock :: Int
lock = -2

unlockDoorKey :: Int
unlockDoorKey = -3

data Function = Lock | Unlock | ChangePin

mkFunction :: Int -> Maybe Function
mkFunction k | k == changePinKey  = Just ChangePin
             | k == lockDoorKey   = Just Lock
             | k == unlockDoorKey = Just Unlock
             | otherwise          = Nothing

data SecureState = SecureState { thepincode :: (Int, Int, Int, Int), doorState :: Bool }

verifyCode :: SRef SecureState -> (Int, Int, Int, Int) -> Secure Bool
verifyCode sr candidateCode = do
    p <- thepincode <$> readSRef sr
    return $ p == candidateCode

unlock :: SRef SecureState -> Secure ()
unlock sr = do
    r <- {-@ foreign call to actual C function that runs the secure door open call @-}
    if r then modifySRef sr $ \st -> st { doorState = True } else return ()

lock :: SRef SecureState -> Secure ()
lock sr = do
    r <- {-@ foreign call to actual C function that runs the secure door close call @-}
    if r then modifySRef sr $ \st -> st { doorState = False } else return ()

-- | Changes the pin code
changePin :: SRef SecureState -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Secure Bool
changePin sr old new = do
    b <- verifyCode sr old
    if b
        then do
            modifySRef sr $ \st -> st { thepincode = new }
            return True
        else return False

unlockDoor :: SRef SecureState -> (Int, Int, Int, Int) -> Secure Bool
unlockDoor sr code = do
    b <- verifyCode sr code
    if b
        then unlock sr >> return True
        else return False

lockDoor :: SRef SecureState -> (Int, Int, Int, Int) -> Secure Bool
lockDoor sr code = do
    b <- verifyCode sr code
    if b
        then lock sr >> return True
        else return False

-- | non secure operation that reads a sequence of 4 numbers from the keypad
readPin :: IO (Maybe (Int, Int, Int, Int))
readPin = go []
  where
    go :: [Int] -> IO (Maybe (Int, Int, Int, Int))
    go (x:y:z:w:_) = return $ Just (x,y,z,w)
    go xs = do
        k <- readKeyPressTimeout 5000
        case k of
            -1 -> return Nothing
            _ -> go (xs ++ [k])

data NSCApi = NSCApi { changeThePin  :: Callable ((Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Secure Bool)
                     , unlockTheDoor :: Callable ((Int, Int, Int, Int) -> Secure Bool)
                     , lockTheDoor   :: Callable ((Int, Int, Int, Int) -> Secure Bool)
                     }

keypad :: NSCApi -> IO ()
keypad api = do
    f <- mkFunction <$> readKeyPress
    case f of
        Just Lock -> do
            pin <- readPin
            case pin of
                Just pin' -> do
                    r <- gateway $ lockTheDoor api <@> pin'
                    if r
                        then putStrLn "success" >> keypad
                        else putStrLn "failure" >> keypad
                Nothing -> keypad

        Just Unlock -> undefined
            pin <- readPin
            case pin of
                Just pin' -> do
                    r <- gateway $ unlockTheDoor api <@> pin'
                    if r
                        then putStrLn "success" >> keypad
                        else putStrLn "failure" >> keypad
                Nothing -> keypad

        Just ChangePin -> do
            pin <- readPin
            case pin of
                Just pin' -> do
                    newpin <- readPin
                    case newpin of
                        Just newpin' -> do
                            r <- gateway $ changeThePin api <@> pin' <@> newpin'
                            if r
                                then putStrLn "success" >> keypad
                                else putStrLn "failure" >> keypad
                        Nothing -> keypad
                Nothing -> keypad

        Nothing -> keypad


-- Here we do all the setup, before we enter the non secure application. We allocate global mutable
-- data in the secure world, mark what is non secure callable, and then enter the non secure
-- application
app :: IO ()
app = do
    securestate <- newSRef $ SecureState (1,2,3,4) False
    ctp <- callable $ changePin     securestate
    ltd <- callable $ lockTheDoor   securestate
    utd <- callable $ unlockTheDoor securestate
    keypad $ NSCApi ctp utd ltd


@-}