{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- The reflexive-weakening hypothesis (Subset xs xs) matches an instance head but
-- cannot be reduced here (its inner membership is stuck on the grade variables).
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | 'hedgehog-classes' law bundles for the graded classes, mirroring
-- @monoidal-functors:laws@.  Phase 1 covers weakening reflexivity for the graded
-- 'ExceptT''; further laws (weakening transitivity/naturality, graded-monad
-- unit/associativity, and the set-grade laws) are added incrementally.
module Control.Monad.Graded.Laws
  ( weakenReflexiveExcept,
  )
where

--------------------------------------------------------------------------------

import Control.Category.Tensor.Expr (Tensored (..))
import Control.Monad.Graded (gweaken)
import Control.Monad.Graded.Except (ExceptT' (..))
import Control.Monad.Graded.Grade (Subset)
import Data.Functor.Identity (Identity (..))
import Data.Void (Void)
import Hedgehog
import Hedgehog.Classes (Laws (..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

--------------------------------------------------------------------------------

-- | @gweaken@ at @xs ⊆ xs@ is the identity, observed through 'runExceptT''.
weakenReflexiveExcept ::
  forall e1 e2.
  (Eq e1, Show e1, Eq e2, Show e2, Subset '[e1, e2] '[e1, e2]) =>
  Gen e1 ->
  Gen e2 ->
  Laws
weakenReflexiveExcept ge1 ge2 =
  Laws
    "Weaken (ExceptT' Identity)"
    [ ( "reflexive weakening is identity",
        property $ do
          v <- forAll genUnderlying
          let m = ExceptT' (Identity v) :: ExceptT' Identity '[e1, e2] Int
          runIt (gweaken m :: ExceptT' Identity '[e1, e2] Int) === runIt m
      )
    ]
  where
    runIt :: ExceptT' Identity es a -> Either (Tensored Either Void es) a
    runIt = runIdentity . runExceptT'

    genUnderlying :: Gen (Either (Tensored Either Void '[e1, e2]) Int)
    genUnderlying =
      Gen.choice
        [ Left . Tensored . Left <$> ge1,
          Left . Tensored . Right . Left <$> ge2,
          Right <$> Gen.int (Range.linear 0 100)
        ]
