module Effectful.TypeLevel.Number where

data Z
data Succ s

class ToInt a where
    -- do not observe the a
    toInt :: proxy a -> Int

instance ToInt Z where
    toInt :: proxy Z -> Int
    toInt _ = 0

instance ToInt a => ToInt (Succ a) where
    toInt :: ToInt a => proxy (Succ a) -> Int
    toInt _ = 1 + (toInt (undefined :: proxy a))

type N0  = Z
type N1  = Succ N0
type N2  = Succ N1
type N3  = Succ N2
type N4  = Succ N3
type N5  = Succ N4
type N6  = Succ N5
type N7  = Succ N6
type N8  = Succ N7
type N9  = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11
type N13 = Succ N12
type N14 = Succ N13
type N15 = Succ N14