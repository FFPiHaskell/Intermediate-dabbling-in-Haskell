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



functorTest1 = P.fmap (P.+ 1) (Right 2) -- sollte Right 3 geben
functorTest2 = P.fmap (P.+ 1) (Left 2)  -- sollte Left 2 geben
applicativeTest1 = A.pure (P.*) A.<*> (Right 2)   A.<*> (Right 2) -- Sollte Right 4 geben
applicativeTest2 = A.pure (P.*) A.<*> (Left "ok") A.<*> (Right 2) -- Sollte Left "ok" geben
mFun1 = (\_ -> Right 3)
mFun2 = (Right P.. (P.+ 3))
mFun3 = (\_ -> Left "ok")
monadTest1 = do               -- sollte Left "ok" geben
                   a <- mFun1 0
                   b <- mFun2 a
                   c <- mFun3 b
                   P.return c

monadTest2 = mFun1 0 P.>>= mFun2 -- sollte Right 6 geben
