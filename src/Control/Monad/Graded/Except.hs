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

import Control.Category.Tensor.Expr
import Control.Monad.Except
import Control.Monad.Graded hiding (return, (>>=))
import Control.Monad.Graded.Except.Class
import Control.Monad.Graded.Grade (Subset (..))
import Data.Bifunctor
import Data.Functor.Identity
import Data.Void

--------------------------------------------------------------------------------

newtype ExceptT' m es a = ExceptT' {runExceptT' :: m (Either (Tensored Either Void es) a)}
  deriving (Functor, Applicative, Monad) via (ExceptT (Tensored Either Void es) m)

deriving via (ExceptT (Tensored Either Void es) m) instance (Monad m) => MonadError (Tensored Either Void es) (ExceptT' m es)

instance (Functor m, Subset xs ys) => Weaken (ExceptT' m) xs ys where
  gweaken (ExceptT' k) = ExceptT' (fmap (first injSub) k)

-- | Grade = the SET of errors that may escape; combine = 'Union' (dedup); the
-- 'gbind' injects whichever side actually raised into the union.
instance (Monad m) => GradedMonad (ExceptT' m) Void Either 'Join where
  greturn = ExceptT' . pure . Right . runIdentity

  gbind (ExceptT' k) f =
    ExceptT' $
      k >>= \case
        Left e -> pure (Left (injSub e))
        Right a ->
          runExceptT' (f a) >>= \case
            Left e' -> pure (Left (injSub e'))
            Right b -> pure (Right b)

instance (Monad m) => GradedMonadError (ExceptT' m) where
  gthrowError :: e -> ExceptT' m '[e] a
  gthrowError e = ExceptT' $ pure $ Left $ Tensored $ Left e

  gcatchError :: ExceptT' m e a -> (Tensored Either Void e -> ExceptT' m e' a) -> ExceptT' m e' a
  gcatchError (ExceptT' m) f =
    ExceptT' $
      m >>= \case
        Left e -> runExceptT' $ f e
        Right a -> pure $ Right a
