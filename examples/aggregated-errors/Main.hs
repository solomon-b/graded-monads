{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE DataKinds #-}
module Main where

import qualified Control.Monad.Graded as G
import Control.Monad.Graded.Except.Class
import Control.Monad.Graded.Except

data HttpError = HttpError deriving Show
data ParseError = ParseError deriving Show
data TransformError = TransformError deriving Show

data Request = Request
data Response = Response deriving Show

mkRequest :: GradedMonadError m => String -> m '[ParseError] Request
mkRequest _ =
  gthrowError ParseError -- G.return Request

transformRequest :: GradedMonadError m => Request -> m '[TransformError] Request
transformRequest _ = gthrowError TransformError

invokeRequest :: GradedMonadError m => Request -> m '[] Response
invokeRequest _ = G.return Response

main :: IO ()
main = do
  result <- runExceptT' $ G.do
    req <- mkRequest "hoogle.hackage.com"
    req' <- transformRequest req
    invokeRequest req'
  print result
  --case result of
  --  Left (Left ParseError) -> print ParseError
  --  Left (Right (Left TransformError)) -> print TransformError
  --  Left (Right (Right HttpError)) -> print HttpError
  --  Right Response -> print Response
