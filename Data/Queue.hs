{-# LANGUAGE NoImplicitPrelude #-}

module Data.Queue (
  Queue,
  empty,
  isEmpty,
  front,
  enqueue,
  dequeue) where

import Data.Bool (Bool)
import Data.Maybe (Maybe)
import Data.Deque hiding (empty, isEmpty, front)
import qualified Data.Deque as D (empty, isEmpty, front)

newtype Queue a = Queue (Deque a)

empty :: Queue a
empty = Queue D.empty

isEmpty :: Queue a -> Bool
isEmpty (Queue d) = D.isEmpty d

front :: Queue a -> Maybe a
front (Queue d) = D.front d

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue d) = Queue (push_back x d)

dequeue :: Queue a -> Queue a
dequeue (Queue d) = Queue (pop_front d)
