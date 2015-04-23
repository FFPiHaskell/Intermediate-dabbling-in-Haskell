module State where

import Control.Applicative --nötig unter ghc 7.8, Warnung bei ghc 7.10

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f rs = State $ \s -> let (a,s') = runState rs s
                            in (f a, s')

instance Applicative (State s) where
    pure a    = State $ \s -> (a,s)
    rf <*> rs = State $ \s ->
                  let (f,s')  = runState rf s
                      (a,s'') = runState rs s'
                  in (f a, s'')

instance Monad (State s) where
    return   = pure
    rs >>= f = State $ \s ->
                let (a,s') = runState rs s
                    rs'    = f a
                in runState rs' s'
