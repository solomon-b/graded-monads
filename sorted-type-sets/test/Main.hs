{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Category.Tensor.Expr (Tensored (..))
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Set (Delete, Insert, Sort, Union, decompose, inj, injSub)
import Data.Void (Void)
import GHC.Generics (Generic)
import System.Exit (exitFailure, exitSuccess)

-- Ordered by datatype name, so A < B < C.
data A = A deriving (Eq, Show, Generic)

data B = B deriving (Eq, Show, Generic)

data C = C deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Value-level open union.

-- 'inj' places a member at its sorted position in the sum.
injPlacesAtPosition :: Bool
injPlacesAtPosition =
  (inj B :: Tensored Either Void '[A, B, C]) == Tensored (Right (Left B))

-- 'injSub' re-indexes a subset value into the larger set.
injSubReindexes :: Bool
injSubReindexes =
  let small = Tensored (Right (Left C)) :: Tensored Either Void '[A, C]
   in (injSub small :: Tensored Either Void '[A, B, C]) == Tensored (Right (Right (Left C)))

-- 'decompose' returns Left for the targeted member.
decomposeHit :: Bool
decomposeHit =
  decompose @B (Tensored (Right (Left B)) :: Tensored Either Void '[A, B, C]) == Left B

-- 'decompose' returns the remaining sum, with the member removed, for anything else.
decomposeMiss :: Bool
decomposeMiss =
  decompose @B (Tensored (Left A) :: Tensored Either Void '[A, B, C])
    == Right (Tensored (Left A) :: Tensored Either Void '[A, C])

main :: IO ()
main = do
  -- Type-level laws. These bindings only type-check if the families reduce as
  -- claimed, so building the test is itself the check.
  let _sortSorts = Refl :: Sort '[C, A, B] :~: '[A, B, C]
      _sortDedups = Refl :: Sort '[B, A, B] :~: '[A, B]
      _unionMergesDedups = Refl :: Union '[C, A] '[A, B] :~: '[A, B, C]
      _insertKeepsSorted = Refl :: Insert B '[A, C] :~: '[A, B, C]
      _deleteRemovesOne = Refl :: Delete B '[A, B, C] :~: '[A, C]
  if and [injPlacesAtPosition, injSubReindexes, decomposeHit, decomposeMiss]
    then exitSuccess
    else exitFailure
