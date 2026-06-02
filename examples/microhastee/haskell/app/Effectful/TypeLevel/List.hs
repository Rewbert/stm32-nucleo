{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
module Effectful.TypeLevel.List (
    Nil,
    Cons,
    Member,
    Delete
) where

import Data.Kind (Type)

data Nil
data Cons (x :: Type) (xs :: Type)

class Member (x :: Type) (xs :: Type)
instance Member x (Cons x xs)
instance (Member x xs) => Member x (Cons y xs)

-- deleting x from xs yields ys
class Delete (x :: Type) (xs :: Type) (ys :: Type)
-- if x is at the head, we can delete it
instance Delete x (Cons x xs) xs
-- if deleting x from xs yields ys, deleting x from (y:xs) yields (y:ys)
instance (Delete x xs ys) => Delete x (Cons y xs) (Cons y ys)