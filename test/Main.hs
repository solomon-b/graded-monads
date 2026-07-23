{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Category.Tensor.Expr (Tensored (..))
import Control.Monad.Graded.Grade (injSub)
import Data.Void (Void)
import System.Exit (exitFailure, exitSuccess)

data E1 = E1 deriving (Eq, Show)

data E2 = E2 deriving (Eq, Show)

main :: IO ()
main = do
  -- injecting the sole error of '[E1] into '[E1, E2] keeps it at position 0
  let injected =
        injSub (Tensored (Left E1) :: Tensored Either Void '[E1])
          :: Tensored Either Void '[E1, E2]
  if injected == Tensored (Left E1)
    then exitSuccess
    else exitFailure
