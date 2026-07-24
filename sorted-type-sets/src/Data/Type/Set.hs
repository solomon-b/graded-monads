{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- | Sorted, canonical type-level sets of types, ordered by a 'GHC.Generics.Generic'-derived
-- name so that membership and union are independent of insertion order.  Values
-- are witnessed as an open union over @monoidal-functors@' @Tensored Either Void@
-- (an n-ary sum).
module Data.Type.Set
  ( -- * Ordering on types
    TypeName,
    Cmp,

    -- * Set algebra
    Insert,
    Sort,
    Set,
    Union,
    Member,
    Subset (..),
    Delete,
    Handle,

    -- * Value-level open union
    inj,
    decompose,
    here,
    there,
    uncons,
  )
where

--------------------------------------------------------------------------------

import Control.Category.Tensor.Expr (Tensored (..), type (++))
import Data.Kind (Type)
import Data.Void (Void, absurd)
import GHC.Generics (D1, Meta (MetaData), Rep)
import GHC.TypeLits (CmpSymbol, Symbol)

--------------------------------------------------------------------------------
-- Ordering on types, from the Generic datatype name.

-- | The datatype name of @a@, as a type-level 'Symbol' (requires @Generic a@).
type TypeName :: Type -> Symbol
type family TypeName a where
  TypeName a = GName (Rep a)

type GName :: (Type -> Type) -> Symbol
type family GName rep where
  GName (D1 ('MetaData name m p nt) f) = name

-- | Total order on types, by 'TypeName'.
type Cmp :: Type -> Type -> Ordering
type family Cmp a b where
  Cmp a b = CmpSymbol (TypeName a) (TypeName b)

--------------------------------------------------------------------------------
-- Sorted insertion, sort, and union (dedup on EQ).

type Insert :: Type -> [Type] -> [Type]
type family Insert x xs where
  Insert x '[] = '[x]
  Insert x (y ': ys) = InsertCmp (Cmp x y) x y ys

type InsertCmp :: Ordering -> Type -> Type -> [Type] -> [Type]
type family InsertCmp o x y ys where
  InsertCmp 'LT x y ys = x ': y ': ys
  InsertCmp 'EQ _ y ys = y ': ys
  InsertCmp 'GT x y ys = y ': Insert x ys

type Sort :: [Type] -> [Type]
type family Sort xs where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)

-- | Set union: sorted and deduplicated, hence independent of the order the two
-- grades were built in.
type Union :: [Type] -> [Type] -> [Type]
type family Union xs ys where
  Union xs ys = Sort (xs ++ ys)

-- | A grade written in any order, normalized to its canonical set.  Handy in
-- signatures: @m (Set '[HttpError, ParseError]) a@ accepts either order.
type Set es = Sort es

--------------------------------------------------------------------------------
-- Value-level open union over  Tensored Either Void.
-- Phrased over the injective 'Tensored' newtype so the list indices stay inferable.

here :: e -> Tensored Either Void (e ': es)
here = Tensored . Left

there :: Tensored Either Void es -> Tensored Either Void (f ': es)
there = Tensored . Right . getTensored

uncons :: Tensored Either Void (x ': xs) -> Either x (Tensored Either Void xs)
uncons t = case getTensored t of
  Left x -> Left x
  Right r -> Right (Tensored r)

--------------------------------------------------------------------------------
-- Membership witness (closed-family position index; no overlapping instances).

data Nat = Z | S Nat

type FindElem :: Type -> [Type] -> Nat
type family FindElem x xs where
  FindElem x (x ': xs) = 'Z
  FindElem x (y ': xs) = 'S (FindElem x xs)

class AtIndex (n :: Nat) (x :: Type) (xs :: [Type]) where
  injAt :: x -> Tensored Either Void xs

instance AtIndex 'Z x (x ': xs) where
  injAt :: x -> Tensored Either Void (x : xs)
  injAt = here

instance (AtIndex n x xs) => AtIndex ('S n) x (y ': xs) where
  injAt :: x -> Tensored Either Void (y : xs)
  injAt = there . injAt @n @x @xs

-- | @x@ is a member of the type-set @xs@.
type Member x xs = AtIndex (FindElem x xs) x xs

-- | Inject a member into the n-ary sum at its position.
inj :: forall x xs. (Member x xs) => x -> Tensored Either Void xs
inj = injAt @(FindElem x xs) @x @xs

--------------------------------------------------------------------------------
-- Subset injection.

-- | @xs@ is a subset of @ys@; embed the smaller sum into the larger.
class Subset xs ys where
  injSub :: Tensored Either Void xs -> Tensored Either Void ys

instance Subset '[] ys where
  injSub :: Tensored Either Void '[] -> Tensored Either Void ys
  injSub = absurd . getTensored

instance (Member x ys, Subset xs ys) => Subset (x ': xs) ys where
  injSub :: Tensored Either Void (x : xs) -> Tensored Either Void ys
  injSub t = case uncons t of
    Left x -> inj x
    Right r -> injSub r

--------------------------------------------------------------------------------
-- Deletion and decomposition.

type DeleteAt :: Nat -> [Type] -> [Type]
type family DeleteAt n xs where
  DeleteAt 'Z (x ': xs) = xs
  DeleteAt ('S n) (y ': xs) = y ': DeleteAt n xs

-- | Remove @e@ from the grade (a set, so its single occurrence).
type Delete e es = DeleteAt (FindElem e es) es

class DecomposeAt (n :: Nat) (e :: Type) (es :: [Type]) where
  decomposeAt :: Tensored Either Void es -> Either e (Tensored Either Void (DeleteAt n es))

instance DecomposeAt 'Z e (e ': es) where
  decomposeAt :: Tensored Either Void (e : es) -> Either e (Tensored Either Void (DeleteAt Z (e : es)))
  decomposeAt t = case uncons t of
    Left e -> Left e
    Right r -> Right r

instance (DecomposeAt n e es) => DecomposeAt ('S n) e (y ': es) where
  decomposeAt :: Tensored Either Void (y : es) -> Either e (Tensored Either Void (DeleteAt (S n) (y : es)))
  decomposeAt t = case uncons t of
    Left y -> Right (here y)
    Right r -> case decomposeAt @n @e r of
      Left e -> Left e
      Right r' -> Right (there r')

-- | The constraint enabling a narrowing project of @e@ from grade @es@.
type Handle e es = DecomposeAt (FindElem e es) e es

-- | Project a member @e@ out of a sum, or return the remaining sum with @e@ removed.
decompose ::
  forall e es.
  (Handle e es) =>
  Tensored Either Void es ->
  Either e (Tensored Either Void (Delete e es))
decompose = decomposeAt @(FindElem e es) @e @es
