{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QualifiedDo #-}
module HW where

import qualified Control.Monad.IxMonad as M

-- type level lists
data Nil
data Cons a as

-- peripherals
data UART

-- type level membership
class Member x xs
instance Member x (Cons x xs)
instance Member x xs => Member x (Cons y xs)

data Z
data S a

type Zero = Z
type One = S Z
type Two = S (S Z)

data A
data B

data GPIO port pin

type RedLed = GPIO A Two

-- class ToInt a where
--     -- do not observe the a
--     toInt :: a -> Int

-- instance ToInt Z where
--     toInt _ = 0

-- instance ToInt a => ToInt (S a) where
--     toInt _ = 1 + (toInt (undefined :: a))

-- -- monad indexed over allowed effects
-- newtype MyMonad cs1 cs2 a = MyMonad (IO a)

-- instance M.IxMonad MyMonad where
--     ireturn = MyMonad . return
--     ibind (MyMonad m) k = MyMonad $ do
--         a <- m
--         let MyMonad iob = k a
--         iob
    
--     ilift ioa = MyMonad ioa

-- unsafeLiftIO :: IO a -> MyMonad cs1 cs2 a
-- unsafeLiftIO ioa = MyMonad ioa

-- -- calling this adds the uart to the allowed list of effects
-- addUart :: MyMonad cs (Cons UART cs) ()
-- addUart = MyMonad $ do
--     undefined -- foreign call goes here, that actually declassifies on the hardware
--     return ()

-- -- printing over uart only allowed if the types say so
-- uartPrint :: (M.IxMonad m, Member UART cbs) => String -> m cbs cbs ()
-- uartPrint str = undefined -- foreign call to call the uart

-- program :: MyMonad Nil (Cons UART Nil) ()
-- program = M.do
--     addUart -- removing this line makes the typechecker discard the 'illegal' program
--     uartPrint "hi"
--     M.return ()