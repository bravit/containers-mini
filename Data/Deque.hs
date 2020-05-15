{-# LANGUAGE NoImplicitPrelude #-}

module Data.Deque (
  Deque,
  empty,
  isEmpty,
  front,
  back,
  push_back,
  push_front,
  pop_back,
  pop_front) where

import Data.Sequence hiding (empty)
import qualified Data.Sequence as Seq
import Data.Bool (Bool)
import Data.Maybe (Maybe(..))

newtype Deque a = Deque (Seq a)

empty ::  Deque a
empty = Deque Seq.empty

isEmpty :: Deque a -> Bool
isEmpty (Deque seq) = null seq

front :: Deque a -> Maybe a
front (Deque (x :<| _)) = Just x
front _ = Nothing

back :: Deque a -> Maybe a
back (Deque (_ :|> x)) = Just x
back _ = Nothing

push_front :: a -> Deque a -> Deque a
push_front x (Deque seq) = Deque (x :<| seq)

push_back :: a -> Deque a -> Deque a
push_back x (Deque seq) = Deque (seq :|> x)

pop_front :: Deque a -> Deque a
pop_front (Deque (_ :<| seq)) = Deque seq

pop_back :: Deque a -> Deque a
pop_back (Deque (seq :|> _)) = Deque seq
