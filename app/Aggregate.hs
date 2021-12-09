module Aggregate where

import Data.Void

class Monad m where
  return :: a -> m (Either Void a)
  (>>=) :: m (Either e1 a) -> (a -> m (Either e2 b)) -> m (Either (Either e1 e2) b)
