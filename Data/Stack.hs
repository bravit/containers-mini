module Data.Stack (Stack, empty, isEmpty, push, pop, top) where

newtype Stack a = Stack [a]

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = null xs

pop :: Stack a -> Stack a
pop (Stack []) = Stack []
pop (Stack (x:xs)) = Stack xs

top :: Stack a -> Maybe a
top (Stack []) = Nothing
top (Stack (x:_)) = Just x
