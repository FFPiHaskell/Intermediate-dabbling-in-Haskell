module Either where

import qualified Prelude as P
import qualified Control.Applicative as A

data Either a b = Left a
                | Right b
                deriving (P.Show, P.Eq)

instance P.Functor (Either a) where
  fmap f e = case e of
               Left a -> Left a
               Right a -> Right (f a)

instance A.Applicative (Either a) where
  pure      = Right
  ef <*> ev = case ef of
              Left err -> Left err
              Right f  -> P.fmap f ev

instance P.Monad (Either a) where
  return    = A.pure
  v >>= f   = case v of
                Left err -> Left err
                Right a  -> f a   
