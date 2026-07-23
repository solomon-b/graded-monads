{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Monad.Graded.Except.Class where

--------------------------------------------------------------------------------

import Control.Category.Tensor.Expr
import Control.Monad.Graded
import Data.Kind
import Data.Void

--------------------------------------------------------------------------------

type GradedMonadError :: ([Type] -> Type -> Type) -> Constraint
class
  ( GradedMonad m Void Either 'Join,
    forall es. Weaken m '[] es
  ) =>
  GradedMonadError m
  where
  gthrowError :: e -> m '[e] a
  gcatchError :: m es a -> (Tensored Either Void es -> m es' a) -> m es' a
