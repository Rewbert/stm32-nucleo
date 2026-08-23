{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Effectful.TypeLevel.List (
    Nil,
    Cons,
    Member,
    Delete,
    Fresh
) where

import Data.Kind (Type)

data Nil
data Cons (x :: Type) (xs :: Type)

class Member (x :: Type) (xs :: Type)
instance Member x (Cons x xs)
instance (Member x xs) => Member x (Cons y xs)

-- deleting x from xs yields ys
class Delete (x :: Type) (xs :: Type) (ys :: Type) | x xs -> ys
-- if x is at the head, we can delete it
instance Delete x (Cons x xs) xs
-- if deleting x from xs yields ys, deleting x from (y:xs) yields (y:ys)
instance (Delete x xs ys) => Delete x (Cons y xs) (Cons y ys)

-- | Uninhabited on purpose: naming it in an instance context makes that branch
-- of instance resolution unsatisfiable. This is what turns the exact-match
-- branch of 'Fresh' into a compile error instead of a silent success -- the
-- same repeated-type-variable trick 'Delete' already relies on to find the
-- head of the list, pushed one step further to make a match a failure instead
-- of a success.
class Fail

-- | @Fresh x xs@ holds iff @x@ does not already occur anywhere in @xs@. Used to
-- reject re-acquiring a peripheral (e.g. a second 'get_gpio' for the same pin)
-- as a compile error rather than silently handing back a second, aliased handle.
class Fresh (x :: Type) (xs :: Type)
instance Fresh x Nil
instance Fail => Fresh x (Cons x xs)
instance (Fresh x xs) => Fresh x (Cons y xs)