{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Graded.Writer where

--------------------------------------------------------------------------------

import Control.Category.Tensor.Expr
import Control.Monad.Graded hiding (return, (>>=))
import Control.Monad.Graded.Writer.Class
import Control.Monad.Writer
import Data.Functor.Compose
import Data.Functor.Identity

--------------------------------------------------------------------------------

newtype WriterT' m w a = WriterT' {runWriterT' :: m (a, Tensored (,) () w)}
  deriving (Functor) via (WriterT (Tensored (,) () w) m)

instance (Monad m) => GradedMonad (WriterT' m) () (,) where
  greturn :: Identity ~> WriterT' m '[]
  greturn (Identity x) = WriterT' $ pure (x, Tensored ())

  gjoin ::
    (AppendTensored xs) =>
    Compose (WriterT' m xs) (WriterT' m ys)
      ~> WriterT' m (xs ++ ys)
  gjoin (Compose (WriterT' mma)) = WriterT' $ do
    (WriterT' ma, logs) <- mma
    (a, logs') <- ma
    pure (a, appendTensored (logs, logs'))

instance (Monad m) => GradedMonadWriter (WriterT' m) where
  gtell :: w -> WriterT' m '[w] ()
  gtell w = WriterT' $ pure ((), Tensored (w, ()))
