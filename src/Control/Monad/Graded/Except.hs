{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
module Control.Monad.Graded.Except where

import Data.Void
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Monad.Except
import Control.Monad.Graded hiding ((>>=), return)
import Control.Monad.Graded.Except.Class
import Data.Bifunctor
import Control.Category.Tensor

newtype ExceptT' m es a = ExceptT' { runExceptT' :: m (Either (Many Either Void es) a) }
  deriving (Functor, Applicative, Monad) via (ExceptT (Many Either Void es) m)

deriving via (ExceptT (Many Either Void es) m) instance Monad m => MonadError (Many Either Void es) (ExceptT' m es)

instance Monad m => GradedMonad (ExceptT' m) Void Either where
  greturn :: Identity ~> ExceptT' m '[]
  greturn  = ExceptT' . pure . Right . runIdentity

  gjoin
    :: AppendMany xs
    => (ExceptT' m xs `Compose` ExceptT' m ys)
    ~> ExceptT' m (xs ++ ys)
  gjoin (Compose (ExceptT' mma))
    = ExceptT'
    $ fmap (first appendMany . fwd assoc)
    $ join
    $ fmap (sequenceA . fmap runExceptT')
    $ mma

instance Monad m => GradedMonadError (ExceptT' m) where
  gthrowError :: e -> ExceptT' m '[e] a
  gthrowError e = ExceptT' $ pure $ Left $ ACons $ Left e

  gcatchError :: ExceptT' m e a -> (Many Either Void e -> ExceptT' m e' a) -> ExceptT' m e' a
  gcatchError (ExceptT' m) f = ExceptT' $ m >>= \case
    Left e -> runExceptT' $ f e
    Right a -> pure $ Right a
