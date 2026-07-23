{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Type.Set (Subset, Union)
import Data.Functor.Identity
import Data.Kind

--------------------------------------------------------------------------------

type (~>) f g = forall x. f x -> g x

type FunctorF :: (k -> Type -> Type) -> Constraint
type FunctorF m = (forall x. Functor (m x))

-- | Weakening along the grade preorder.  Each graded monad declares its own
-- order and coercion: errors weaken along @⊆@, writer along equality.
type Weaken :: ([Type] -> Type -> Type) -> [Type] -> [Type] -> Constraint
class Weaken m xs ys where
  gweaken :: m xs a -> m ys a

--------------------------------------------------------------------------------

-- | The monoid a graded monad uses to combine grades: the free monoid (@++@,
-- ordered, keeps duplicates) or the join-semilattice (@Union@, a set).
data GradeAlgebra = Free | Join

-- | How two grades combine under @alg@.
type Combine :: GradeAlgebra -> [Type] -> [Type] -> [Type]
type family Combine alg xs ys where
  Combine 'Free xs ys = xs ++ ys
  Combine 'Join xs ys = Union xs ys

-- | The constraint 'gbind' needs in order to combine grades under @alg@.
type CombineC :: GradeAlgebra -> [Type] -> [Type] -> Constraint
type family CombineC alg xs ys where
  CombineC 'Free xs ys = AppendTensored xs
  CombineC 'Join xs ys = (Subset xs (Union xs ys), Subset ys (Union xs ys))

type GradedMonad ::
  ([Type] -> Type -> Type) ->
  Type ->
  (Type -> Type -> Type) ->
  GradeAlgebra ->
  Constraint
class (FunctorF m) => GradedMonad m i t alg | m -> i t alg where
  greturn :: Identity ~> m '[]
  gbind :: (CombineC alg xs ys) => m xs a -> (a -> m ys b) -> m (Combine alg xs ys) b

--------------------------------------------------------------------------------

return ::
  (GradedMonad m i t alg, Weaken m '[] es) =>
  x ->
  m es x
return = gweaken . greturn . Identity

(>>=) ::
  (GradedMonad m i t alg, CombineC alg xs ys) =>
  m xs a ->
  (a -> m ys b) ->
  m (Combine alg xs ys) b
(>>=) = gbind

(>>) ::
  (GradedMonad m i t alg, CombineC alg xs ys) =>
  m xs a ->
  m ys b ->
  m (Combine alg xs ys) b
(>>) ma mb = gbind ma (const mb)
