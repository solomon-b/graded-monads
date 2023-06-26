{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Graded.Except where

--------------------------------------------------------------------------------

import Control.Category.Tensor
import Control.Category.Tensor.Expr
import Control.Monad.Except
import Control.Monad.Graded hiding (return, (>>=))
import Control.Monad.Graded.Except.Class
import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Void

--------------------------------------------------------------------------------

newtype ExceptT' m es a = ExceptT' {runExceptT' :: m (Either (Tensored Either Void es) a)}
  deriving (Functor, Applicative, Monad) via (ExceptT (Tensored Either Void es) m)

deriving via (ExceptT (Tensored Either Void es) m) instance (Monad m) => MonadError (Tensored Either Void es) (ExceptT' m es)

instance (Monad m) => GradedMonad (ExceptT' m) Void Either where
  greturn :: Identity ~> ExceptT' m '[]
  greturn = ExceptT' . pure . Right . runIdentity

  gjoin ::
    (AppendTensored xs) =>
    (ExceptT' m xs `Compose` ExceptT' m ys)
      ~> ExceptT' m (xs ++ ys)
  gjoin (Compose (ExceptT' mma)) =
    ExceptT' $
      fmap (first appendTensored . fwd assoc) (traverse runExceptT' =<< mma)

instance (Monad m) => GradedMonadError (ExceptT' m) where
  gthrowError :: e -> ExceptT' m '[e] a
  gthrowError e = ExceptT' $ pure $ Left $ Tensored $ Left e

  gcatchError :: ExceptT' m e a -> (Tensored Either Void e -> ExceptT' m e' a) -> ExceptT' m e' a
  gcatchError (ExceptT' m) f =
    ExceptT' $
      m >>= \case
        Left e -> runExceptT' $ f e
        Right a -> pure $ Right a
