{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
module Control.Monad.Graded.Writer.Class where

import Control.Monad.Graded

class GradedMonad m () (,) => GradedMonadWriter m
  where
  gtell :: w -> m '[w] ()
