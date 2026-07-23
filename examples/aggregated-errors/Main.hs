{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}

module Main where

--------------------------------------------------------------------------------

import Control.Category.Tensor.Expr (getTensored)
import qualified Control.Monad.Graded as G
import Control.Monad.Graded.Except
import Control.Monad.Graded.Except.Class
import Data.Void (absurd)

--------------------------------------------------------------------------------

data ParseError = ParseError deriving (Show)

data TransformError = TransformError deriving (Show)

data Request = Request

data Response = Response deriving (Show)

--------------------------------------------------------------------------------

mkRequest :: (GradedMonadError m) => String -> m '[ParseError] Request
mkRequest _ = gthrowError ParseError

-- Previously rejected: a fallible-typed function whose body succeeds.  'G.return'
-- weakens the empty grade into the declared '[TransformError].
transformRequest :: (GradedMonadError m) => Request -> m '[TransformError] Request
transformRequest _ = G.return Request

invokeRequest :: (GradedMonadError m) => Request -> m '[] Response
invokeRequest _ = G.return Response

program :: (GradedMonadError m) => m '[ParseError, TransformError] Response
program = G.do
  req <- mkRequest "hoogle.hackage.com"
  req' <- transformRequest req
  invokeRequest req'

-- Partial handling: recover ParseError, propagate TransformError.  The branches
-- unify at  m '[TransformError] Response  via weakening.
recoverParse ::
  (GradedMonadError m) =>
  m '[ParseError, TransformError] Response ->
  m '[TransformError] Response
recoverParse p = gcatchError p $ \err -> case getTensored err of
  Left ParseError -> G.return Response
  Right (Left TransformError) -> gthrowError TransformError
  Right (Right v) -> absurd v

main :: IO ()
main = do
  r1 <- runExceptT' program
  putStrLn ("program           = " ++ show r1)
  r2 <- runExceptT' (recoverParse program)
  putStrLn ("recoverParse prog = " ++ show r2)
