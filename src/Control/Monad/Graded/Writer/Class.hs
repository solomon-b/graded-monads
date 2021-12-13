{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Graded.Writer.Class where

import Control.Monad.Graded

-- TODO: What do we actually want in this class?
class GradedMonad m () (,) => GradedMonadWriter m where
  -- {-# MINIMAL (gwriter | gtell), glisten, gpass #-}
  --gwriter :: (a, w) -> m w a
  gtell   :: w -> m w ()
  -- gtell w = writer ((), w)
  --glisten :: m w a -> m w (t a w)
  --gpass   :: m w (t a (w -> w)) -> m w a

--glistens :: GradedMonadWriter w m => (w -> b) -> m a -> m (a, b) 
