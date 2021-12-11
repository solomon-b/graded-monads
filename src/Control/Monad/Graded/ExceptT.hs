{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Graded.ExceptT where

import Data.Void
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Monad.Graded hiding ((>>=), return)
import Control.Monad.Except

newtype ExceptT' m e a = ExceptT' { runExceptT' :: m (Either e a) }
  deriving (Functor, Applicative, Monad) via  (ExceptT e m)

instance Monad m => GradedMonad (ExceptT' m) Void Either where
  greturn :: Identity ~> ExceptT' m Void
  greturn  = ExceptT' . pure . Right . runIdentity

  gjoin :: Compose (ExceptT' m x) (ExceptT' m y) ~> ExceptT' m (Either x y) 
  gjoin (Compose (ExceptT' m)) = ExceptT' $ m >>= \case
    Left x -> pure $ Left $ Left x
    Right m1 -> runExceptT' m1 >>= \case
      Left y -> pure $ Left $ Right y
      Right a -> pure $ Right a

