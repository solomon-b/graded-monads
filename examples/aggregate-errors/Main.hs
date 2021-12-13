{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}
module Main where

import qualified Control.Monad.Graded as Graded
import Control.Monad.Graded.Except

import Control.Monad.Except

data HttpError = HttpError deriving Show
data ParseError = ParseError deriving Show
data TransformError = TransformError deriving Show

data Request = Request
data Response = Response deriving Show

mkRequest :: MonadError ParseError m => String -> m Request
mkRequest _ = throwError ParseError -- pure $ Request

transformRequest :: MonadError TransformError m => Request -> m Request
transformRequest _ = pure Request

invokeRequest :: MonadError HttpError m => Request -> m Response
invokeRequest _ = pure Response

main :: IO ()
main = do
  result <- runExceptT' $ Graded.do
    req <- mkRequest "hoogle.hackage.com"
    req' <- transformRequest req
    invokeRequest req'
  case result of
    Left (Left ParseError) -> print ParseError
    Left (Right (Left TransformError)) -> print TransformError
    Left (Right (Right HttpError)) -> print HttpError
    Right Response -> print Response
