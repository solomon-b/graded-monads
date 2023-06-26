{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Control.Monad.Graded.Writer.Class where

--------------------------------------------------------------------------------

import Control.Monad.Graded
import Data.Kind

--------------------------------------------------------------------------------

type GradedMonadWriter :: ([Type] -> Type -> Type) -> Constraint
class (GradedMonad m () (,)) => GradedMonadWriter m where
  gtell :: w -> m '[w] ()

-- type GradedMonadWriter2 :: [Type] -> (Type -> Type) -> Constraint
-- class GradedMonadWriter2 ws m
--  where
--  gtell' :: w `In` ws => w -> m ws ()
--
-- class In
