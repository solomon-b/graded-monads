{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}

module Main where

--------------------------------------------------------------------------------

import qualified Control.Monad.Graded as G
import Control.Monad.Graded.Except
import Control.Monad.Graded.Except.Class

--------------------------------------------------------------------------------

data HttpError = HttpError deriving (Show)

data ParseError = ParseError deriving (Show)

data TransformError = TransformError deriving (Show)

data Request = Request deriving (Show)

data Response = Response deriving (Show)

--------------------------------------------------------------------------------

-- A subroutine with a real failure path AND a success path, both at '[ParseError].
-- The success branch type-checks because 'return' weakens '[] into '[ParseError].
mkRequest :: (GradedMonadError m) => String -> m '[ParseError] Request
mkRequest host
  | null host = gthrowError ParseError
  | otherwise = G.return Request

transformRequest :: (GradedMonadError m) => Request -> m '[TransformError] Request
transformRequest _ = G.return Request

invokeRequest :: (GradedMonadError m) => Request -> m '[HttpError] Response
invokeRequest _ = gthrowError HttpError

program :: (GradedMonadError m) => m '[ParseError, TransformError, HttpError] Response
program = G.do
  req <- mkRequest "hoogle.hackage.com"
  req' <- transformRequest req
  invokeRequest req'

-- Both attempts may raise ParseError; the set grade dedups to a single
-- '[ParseError] (the old list grade would give '[ParseError, ParseError]).
retry :: (GradedMonadError m) => m '[ParseError] Request
retry = G.do
  _ <- mkRequest ""
  mkRequest "fallback"

main :: IO ()
main = do
  r <- runExceptT' program
  putStrLn ("program = " ++ show r)
  r2 <- runExceptT' retry
  putStrLn ("retry   = " ++ show r2)
