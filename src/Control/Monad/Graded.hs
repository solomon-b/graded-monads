{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Graded where

--------------------------------------------------------------------------------

import Control.Category.Tensor.Expr
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind

--------------------------------------------------------------------------------

type (~>) f g = forall x. f x -> g x

type FunctorF :: (k -> Type -> Type) -> Constraint
type FunctorF m = (forall x. Functor (m x))

type GradedMonad ::
  ([Type] -> Type -> Type) ->
  Type ->
  (Type -> Type -> Type) ->
  Constraint
class (FunctorF m) => GradedMonad m i t | m -> i t where
  {-# MINIMAL greturn, (gjoin | gbind) #-}
  greturn :: Identity ~> m '[]

  gjoin :: (AppendTensored xs) => (m xs `Compose` m ys) ~> m (xs ++ ys)
  gjoin (Compose m) = m `gbind` id

  gbind :: (AppendTensored xs) => m xs a -> (a -> m ys b) -> m (xs ++ ys) b
  gbind mxa f = gjoin $ Compose $ fmap f mxa

return ::
  (GradedMonad m i t) =>
  x ->
  m '[] x
return = greturn . Identity

(>>=) ::
  (GradedMonad m i t) =>
  (AppendTensored xs) =>
  m xs a ->
  (a -> m ys b) ->
  m (xs ++ ys) b
(>>=) = gbind

(>>) ::
  (GradedMonad m i t) =>
  (AppendTensored xs) =>
  m xs a ->
  m ys b ->
  m (xs ++ ys) b
(>>) ma mb = gbind ma (const mb)
