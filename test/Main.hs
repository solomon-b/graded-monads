{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Category.Tensor.Expr (Tensored (..))
import Control.Monad.Graded (gweaken)
import Control.Monad.Graded.Except (ExceptT' (..))
import Control.Monad.Graded.Grade (Union, injSub)
import Control.Monad.Graded.Laws (weakenReflexiveExcept)
import Data.Functor.Identity (Identity (..))
import Data.Void (Void)
import Hedgehog.Classes (lawsCheck)
import qualified Hedgehog.Gen as Gen
import System.Exit (exitFailure, exitSuccess)

data E1 = E1 deriving (Eq, Show)

data E2 = E2 deriving (Eq, Show)

-- weakening a computation that raised E1 into the larger set '[E1, E2]
-- keeps the error value intact.
weakenCheck :: Bool
weakenCheck =
  let m = ExceptT' (Identity (Left (Tensored (Left E1)))) :: ExceptT' Identity '[E1] Int
      w = gweaken m :: ExceptT' Identity '[E1, E2] Int
   in runIdentity (runExceptT' w) == Left (Tensored (Left E1))

-- Union dedups at the type level: '[E1] ∪ '[E1] must reduce to '[E1], so these
-- two values share a type and are comparable (it would not type-check otherwise).
unionDedupCheck :: Bool
unionDedupCheck =
  let x = Tensored (Left E1) :: Tensored Either Void (Union '[E1] '[E1])
      y = Tensored (Left E1) :: Tensored Either Void '[E1]
   in x == y

main :: IO ()
main = do
  lawsOk <- lawsCheck (weakenReflexiveExcept (Gen.constant E1) (Gen.constant E2))
  if injected == Tensored (Left E1) && weakenCheck && unionDedupCheck && lawsOk
    then exitSuccess
    else exitFailure
  where
    injected =
      injSub (Tensored (Left E1) :: Tensored Either Void '[E1])
        :: Tensored Either Void '[E1, E2]
