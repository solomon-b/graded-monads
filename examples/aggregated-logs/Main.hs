{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}

module Main where

--------------------------------------------------------------------------------

import qualified Control.Monad.Graded as G
import Control.Monad.Graded.Writer
import Control.Monad.Graded.Writer.Class

--------------------------------------------------------------------------------

data HttpLog = HttpLog deriving (Show)

data ParseLog = ParseLog deriving (Show)

data TransformLog = TransformLog deriving (Show)

data Request = Request

data Response = Response deriving (Show)

mkRequest :: (GradedMonadWriter m) => String -> m '[ParseLog] Request
mkRequest _ = G.do
  gtell ParseLog
  G.return Request

transformRequest :: (GradedMonadWriter m) => Request -> m '[TransformLog] Request
transformRequest _ = G.do
  gtell TransformLog
  G.return Request

invokeRequest :: (GradedMonadWriter m) => Request -> m '[HttpLog] Response
invokeRequest _ = G.do
  gtell HttpLog
  G.return Response

main :: IO ()
main = do
  result <- runWriterT' $ G.do
    req <- mkRequest "hoogle.hackage.com"
    req' <- transformRequest req
    invokeRequest req'
  print result

-- case result of
--  Left (Left ParseError) -> print ParseError
--  Left (Right (Left TransformError)) -> print TransformError
--  Left (Right (Right HttpError)) -> print HttpError
--  Right Response -> print Response
