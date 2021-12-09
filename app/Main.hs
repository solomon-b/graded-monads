{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Void
import qualified Aggregate

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

instance Aggregate.Monad IO where
  return :: a -> IO (Either Void a)
  return a = pure $ Right a

  (>>=) :: IO (Either e1 a) -> (a -> IO (Either e2 b)) -> IO (Either (Either e1 e2) b)
  (>>=) m f = m >>= \case
    Left e1 -> pure $ Left $ Left e1
    Right a -> f a >>= \case
      Left e2 -> pure $ Left $ Right e2
      Right b -> pure $ Right b

main :: IO ()
main = do
  result <- Aggregate.do
    req <- mkRequest "hoogle.hackage.com"
    req' <- transformRequest req
    invokeRequest req'
  case result of
    Left (Left ParseError) -> print ParseError
    Left (Right (Left TransformError)) -> print TransformError
    Left (Right (Right HttpError)) -> print HttpError
    Right Response -> print Response
