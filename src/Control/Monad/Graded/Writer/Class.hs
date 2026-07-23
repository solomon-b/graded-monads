{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Control.Monad.Graded.Writer.Class where

--------------------------------------------------------------------------------

import Control.Monad.Graded
import Data.Kind

--------------------------------------------------------------------------------

type GradedMonadWriter :: ([Type] -> Type -> Type) -> Constraint
class (GradedMonad m () (,) 'Free, Weaken m '[] '[]) => GradedMonadWriter m where
  gtell :: w -> m '[w] ()
