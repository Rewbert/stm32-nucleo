module Effectful.TypeLevel.List (
    Nil,
    Cons
) where

data Nil
data Cons x xs

class Member x xs
instance Member x (Cons x xs)
instance (Member x xs) => Member x (Cons y xs)