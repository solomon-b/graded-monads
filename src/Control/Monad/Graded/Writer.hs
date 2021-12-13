{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Control.Monad.Graded.Writer where

import Data.Functor.Identity
import Data.Functor.Compose
import Control.Monad.Graded hiding ((>>=), return)
import Control.Monad.Graded.Writer.Class
import Control.Monad.Writer

newtype WriterT' m w a = WriterT' { runWriterT' :: (m (a, w)) }
  deriving (Functor, Applicative, Monad) via  (WriterT w m)

instance Monad m => GradedMonad (WriterT' m) () (,) where
  greturn:: Identity ~> WriterT' m ()
  greturn (Identity x) = WriterT' $ pure (x, ())

  gjoin :: Compose (WriterT' m x) (WriterT' m y) x1 -> WriterT' m (x, y) x1
  gjoin (Compose (WriterT' m)) = WriterT' $ m >>= 
      \(WriterT' m1, x) -> fmap (x,) <$> m1

instance (Monoid w, Monad m) => MonadWriter w (WriterT' m w) where
  writer :: (a, w) -> WriterT' m w a
  writer (a, w) = do
    tell w
    pure a

  tell :: w -> WriterT' m w ()
  tell w = WriterT' $ pure ((), w)

  listen :: WriterT' m w a -> WriterT' m w (a, w)
  listen (WriterT' m) = WriterT' $ m >>= \(a, w) -> pure ((a, w), w)

  pass :: WriterT' m w (a, w -> w) -> WriterT' m w a
  pass (WriterT' m) = WriterT' $ m >>= \((a, f), w) -> pure (a, f w)

instance Monad m => GradedMonadWriter (WriterT' m) where
  gtell :: w -> WriterT' m w ()
  gtell w = WriterT' $ pure ((), w)
