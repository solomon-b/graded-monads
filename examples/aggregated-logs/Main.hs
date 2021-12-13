{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}
module Main where

import Data.Functor.Identity
import qualified Control.Monad.Graded as Graded
import Control.Monad.Graded.Writer
import Control.Monad.Graded.Writer.Class

data HttpLog = HttpLog deriving Show
data ParseLog = ParseLog deriving Show
data TransformLog = TransformLog deriving Show

data Request = Request
data Response = Response deriving Show

mkRequest :: GradedMonadWriter m => String -> m (HttpLog, ()) Request
mkRequest _ = Graded.do
  gtell HttpLog
  Graded.return $ Request

--transformRequest :: GradedMonadWriter m => Request -> m Request
--transformRequest _ = Graded.greturn Request
--
--invokeRequest :: GradedMonadWriter m => Request -> m Response
--invokeRequest _ = Graded.greturn Response

main :: IO ()
main =  print "hi" --do
  --result <- runWriterT' $ Graded.do
  --  req <- mkRequest "hoogle.hackage.com"
  --  req' <- transformRequest req
  --  invokeRequest req'
  --print result
  --case result of
  --  Left (Left ParseError) -> print ParseError
  --  Left (Right (Left TransformError)) -> print TransformError
  --  Left (Right (Right HttpError)) -> print HttpError
  --  Right Response -> print Response
