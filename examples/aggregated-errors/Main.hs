{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}

module Main where

--------------------------------------------------------------------------------

import qualified Control.Monad.Graded as G
import Control.Monad.Graded.Except
import Control.Monad.Graded.Except.Class
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data HttpError = HttpError deriving (Show, Generic)

data ParseError = ParseError deriving (Show, Generic)

data TransformError = TransformError deriving (Show, Generic)

data Request = Request deriving (Show, Generic)

data Response = Response deriving (Show, Generic)

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

-- The grade is a set, kept in canonical (type-name) order, so it no longer
-- depends on the order these subroutines run in.
program :: (GradedMonadError m) => m '[HttpError, ParseError, TransformError] Response
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
