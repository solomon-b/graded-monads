{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Category.Tensor.Expr (Tensored (..))
import Control.Monad.Graded (gweaken)
import Control.Monad.Graded.Except (ExceptT' (..))
import Control.Monad.Graded.Grade (injSub)
import Data.Functor.Identity (Identity (..))
import Data.Void (Void)
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

main :: IO ()
main =
  if injected == Tensored (Left E1) && weakenCheck
    then exitSuccess
    else exitFailure
  where
    injected =
      injSub (Tensored (Left E1) :: Tensored Either Void '[E1])
        :: Tensored Either Void '[E1, E2]
