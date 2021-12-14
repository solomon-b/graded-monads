{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
module Control.Monad.Graded.Except.Class where

import Control.Category.Tensor.Expr
import Control.Monad.Graded
import Data.Void

class GradedMonad m Void Either => GradedMonadError m
  where
  gthrowError :: e -> m '[e] a
  gcatchError :: m es a -> (Tensored Either Void es -> m es' a) -> m es' a
