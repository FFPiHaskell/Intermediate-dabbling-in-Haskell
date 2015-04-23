module Identity where

import Control.Applicative --nötig für ghc 7.8, Warnung bei 7.10

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f i = Identity . f . runIdentity $ i

instance Applicative Identity where
  pure     = Identity
  fi <*> i = fmap (runIdentity fi) i

instance Monad Identity where
  return    = pure
  i >>= fi  = fi $ runIdentity i
