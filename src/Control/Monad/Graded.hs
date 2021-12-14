{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Control.Monad.Graded where

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind
import Control.Category.Tensor

type (~>) f g = forall x. f x -> g x

type FunctorF :: (k -> Type -> Type) -> Constraint
type FunctorF m = (forall x. Functor (m x))

type Many
  :: (Type -> Type -> Type)
  -> Type
  -> [Type]
  -> Type
data Many t i vs
  where
  ANil :: i -> Many t i '[]
  ACons :: t v (Many t i vs) -> Many t i (v ': vs)

instance Show i => Show (Many t i '[])
  where
  show = \case
    ANil i -> show i

instance
  ( forall a b. (Show a, Show b) => Show (t a b)
  , Show x
  , Show (Many t i xs)
  ) =>
  Show (Many t i (x ': xs))
  where
  show = \case
    ACons xxs -> show xxs

type (++) :: [k] -> [k] -> [k]
type family xs ++ ys
  where
  '[] ++ xs = xs
  (x ': xs) ++ ys = x ': (xs ++ ys)

class AppendMany xs
  where
  appendMany
    :: Tensor t i (->)
    => Many t i xs `t` Many t i ys
    -> Many t i (xs ++ ys)

instance AppendMany '[]
  where
  appendMany = fwd lunit . glmap (\case { ANil i -> i })

instance AppendMany xs => AppendMany (x ': xs)
  where
  appendMany = ACons . grmap appendMany . bwd assoc . glmap (\case { ACons i -> i })

type GradedMonad
  :: ([Type] -> Type -> Type)
  -> Type
  -> (Type -> Type -> Type)
  -> Constraint
class FunctorF m => GradedMonad m i t | m -> i t where
  {-# MINIMAL greturn, (gjoin | gbind) #-}
  greturn :: Identity ~> m '[]

  gjoin :: AppendMany xs => (m xs `Compose` m ys) ~> m (xs ++ ys)
  gjoin (Compose m) = m `gbind` id

  gbind :: AppendMany xs => m xs a -> (a -> m ys b) -> m (xs ++ ys) b
  gbind mxa f = gjoin $ Compose $ fmap f mxa

return
  :: GradedMonad m i t
  => x
  -> m '[] x
return = greturn . Identity

(>>=)
  :: GradedMonad m i t
  => AppendMany xs
  => m xs a
  -> (a -> m ys b)
  -> m (xs ++ ys) b
(>>=) = gbind

(>>)
  :: GradedMonad m i t
  => AppendMany xs
  => m xs a
  -> m ys b
  -> m (xs ++ ys) b
(>>) ma mb = gbind ma (const mb)
