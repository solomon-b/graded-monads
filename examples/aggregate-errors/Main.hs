{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

import qualified Control.Monad.Graded as Graded
import Control.Monad.Graded.ExceptT

data HttpError = HttpError deriving Show
data ParseError = ParseError deriving Show
data TransformError = TransformError deriving Show

data Request = Request
data Response = Response deriving Show

mkRequest :: Monad m => String -> m (Either ParseError Request)
mkRequest _ = pure $ Right Request

transformRequest :: Monad m => Request -> m (Either TransformError Request)
transformRequest _ = pure $ Right Request

invokeRequest :: Monad m => Request -> m (Either HttpError Response)
invokeRequest _ = pure $ Right Response

main :: IO ()
main = do
  result <- runExceptT' $ Graded.do
    req <- ExceptT' $ mkRequest "hoogle.hackage.com"
    req' <- ExceptT' $ transformRequest req
    ExceptT' $ invokeRequest req'
  case result of
    Left (Left ParseError) -> print ParseError
    Left (Right (Left TransformError)) -> print TransformError
    Left (Right (Right HttpError)) -> print HttpError
    Right Response -> print Response
