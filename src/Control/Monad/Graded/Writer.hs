{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Graded.Writer where

--------------------------------------------------------------------------------

import Control.Category.Tensor.Expr
import Control.Monad.Graded hiding (return, (>>=))
import Control.Monad.Graded.Writer.Class
import Control.Monad.Writer
import Data.Functor.Identity

--------------------------------------------------------------------------------

newtype WriterT' m w a = WriterT' {runWriterT' :: m (a, Tensored (,) () w)}
  deriving (Functor) via (WriterT (Tensored (,) () w) m)

instance (xs ~ ys) => Weaken (WriterT' m) xs ys where
  gweaken = id

-- | Grade = the LIST of logs; combine = @++@ (ordered, keeps duplicates).
instance (Monad m) => GradedMonad (WriterT' m) () (,) 'Free where
  greturn (Identity x) = WriterT' $ pure (x, Tensored ())

  gbind (WriterT' mma) f = WriterT' $ do
    (a, logs) <- mma
    (b, logs') <- runWriterT' (f a)
    pure (b, appendTensored (logs, logs'))

instance (Monad m) => GradedMonadWriter (WriterT' m) where
  gtell :: w -> WriterT' m '[w] ()
  gtell w = WriterT' $ pure ((), Tensored (w, ()))
