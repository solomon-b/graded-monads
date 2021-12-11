{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Graded where

import Data.Functor.Identity
import Data.Functor.Compose
import Data.Void

type (~>) f g = forall x. f x -> g x

class (forall x. Functor (m x)) => GradedMonad m i t | m -> i t where
  {-# MINIMAL greturn, (gjoin | gbind) #-}
  greturn :: Identity ~> m i 

  gjoin :: Compose (m x) (m y) ~> m (t x y)
  gjoin (Compose m) = m `gbind` id

  gbind :: m x a -> (a -> m y b) -> m (t x y) b 
  gbind mxa f = gjoin $ Compose $ fmap f mxa

return :: GradedMonad m i t => x -> m i x
return = greturn . Identity

(>>=) :: GradedMonad m i t => m x a -> (a -> m y b) -> m (t x y) b
(>>=) = gbind

instance GradedMonad Either Void Either where
  greturn :: Identity ~> Either Void
  greturn (Identity x) = Right x

  gjoin :: Compose (Either x) (Either y) ~> Either (Either x y)
  gjoin (Compose f) =
    case f of
      Left x -> Left (Left x)
      Right (Left y) -> Left (Right y)
      Right (Right a) -> Right a
