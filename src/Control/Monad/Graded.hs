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
{-# LANGUAGE UndecidableInstances #-}
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

instance (AsNested t i xs r, Show r) => Show (Many t i xs)
  where
  show = show . fwd asNested

type AsNested :: (Type -> Type -> Type) -> Type -> [Type] -> Type -> Constraint
class Tensor t i (->) => AsNested t i xs unwrapped | t i xs -> unwrapped
  where
  asNested :: Tensor t i (->) => Iso (->) (Many t i xs) unwrapped

instance Tensor t i (->) => AsNested t i '[] i
  where
  asNested :: Tensor t i (->) => Iso (->) (Many t i '[]) i
  asNested = Iso to from
    where
      to :: Tensor t i (->) => Many t i '[] -> i
      to = \case
        ANil i -> i
      from :: Tensor t i (->) => i -> Many t i '[]
      from = ANil 
  
instance (Tensor t i (->), AsNested t i xs r) => AsNested t i (x ': xs) (x `t` r)
  where
  asNested :: Tensor t i (->) => Iso (->) (Many t i (x ': xs)) (x `t` r)
  asNested = Iso to from
    where
      to :: (AsNested t i xs r, Tensor t i (->)) => Many t i (x ': xs) -> (x `t` r)
      to = \case
        ACons xxs -> grmap (fwd asNested) xxs
      from :: (AsNested t i xs r, Tensor t i (->)) => (x `t` r) -> Many t i (x ': xs)
      from xtr = ACons $ grmap (bwd asNested) xtr

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
