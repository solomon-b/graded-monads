{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Graded.Except.Class where

import Control.Monad.Graded
import Data.Void

class GradedMonad m Void Either => GradedMonadError m where
    gthrowError :: e -> m e a
    gcatchError :: m e a -> (e -> m e a) -> m e a 
