{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The grade set-algebra for errors: membership and the subset injection that
-- embeds a smaller error sum into a larger one.  Phrased over the injective
-- 'Tensored' newtype (not the non-injective 'MConcat') so the list indices stay
-- inferable, and driven by a closed-family position witness so nothing relies on
-- overlapping instances.
module Control.Monad.Graded.Grade
  ( Member,
    inj,
    Subset (..),
    here,
    there,
    uncons,
  )
where

--------------------------------------------------------------------------------

import Control.Category.Tensor.Expr (Tensored (..))
import Data.Kind (Type)
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------

-- Injective helpers: signatures pin the list indices through 'Tensored'.

here :: e -> Tensored Either Void (e ': es)
here = Tensored . Left

there :: Tensored Either Void es -> Tensored Either Void (f ': es)
there = Tensored . Right . getTensored

uncons :: Tensored Either Void (x ': xs) -> Either x (Tensored Either Void xs)
uncons t = case getTensored t of
  Left x -> Left x
  Right r -> Right (Tensored r)

--------------------------------------------------------------------------------

-- Closed-family position witness (no overlapping instances).

data Nat = Z | S Nat

type family FindElem (x :: Type) (xs :: [Type]) :: Nat where
  FindElem x (x ': xs) = 'Z
  FindElem x (y ': xs) = 'S (FindElem x xs)

class AtIndex (n :: Nat) (x :: Type) (xs :: [Type]) where
  injAt :: x -> Tensored Either Void xs

instance AtIndex 'Z x (x ': xs) where
  injAt = here

instance (AtIndex n x xs) => AtIndex ('S n) x (y ': xs) where
  injAt = there . injAt @n @x @xs

-- | @x@ is a member of the type-set @xs@.
type Member x xs = AtIndex (FindElem x xs) x xs

-- | Inject a single member into the n-ary error sum at its position.
inj :: forall x xs. (Member x xs) => x -> Tensored Either Void xs
inj = injAt @(FindElem x xs) @x @xs

--------------------------------------------------------------------------------

-- | @xs@ is a subset of @ys@; embed the smaller error sum into the larger.
class Subset xs ys where
  injSub :: Tensored Either Void xs -> Tensored Either Void ys

instance Subset '[] ys where
  injSub = absurd . getTensored

instance (Member x ys, Subset xs ys) => Subset (x ': xs) ys where
  injSub t = case uncons t of
    Left x -> inj x
    Right r -> injSub r
