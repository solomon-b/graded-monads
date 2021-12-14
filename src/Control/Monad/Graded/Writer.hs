{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Control.Monad.Graded.Writer where

import Data.Functor.Identity
import Data.Functor.Compose
import Control.Monad.Graded hiding ((>>=), return)
import Control.Monad.Graded.Writer.Class
import Control.Monad.Writer

newtype WriterT' m w a = WriterT' { runWriterT' :: (m (a, Many (,) () w)) }
  deriving (Functor) via  (WriterT (Many (,) () w) m)

instance Monad m => GradedMonad (WriterT' m) () (,)
  where
  greturn:: Identity ~> WriterT' m '[]
  greturn (Identity x) = WriterT' $ pure (x, ANil ())

  gjoin
    :: AppendMany xs
    => Compose (WriterT' m xs) (WriterT' m ys)
    ~> WriterT' m (xs ++ ys)
  gjoin (Compose (WriterT' mma)) = WriterT' $ do
    (WriterT' ma, logs :: _) <- mma
    (a, logs' :: _) <- ma
    pure $ (a, appendMany (logs, logs'))

instance Monad m => GradedMonadWriter (WriterT' m)
  where
  gtell :: w -> WriterT' m '[w] ()
  gtell w = WriterT' $ pure ((), ACons (w, ANil ()))
