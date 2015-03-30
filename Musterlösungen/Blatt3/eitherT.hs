module Main where

import Data.Functor
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift,MonadTrans)
import Control.Monad (liftM)


data EitherT l m r = EitherT { runEitherT :: m (Either l r) }

instance Functor f => Functor (EitherT l f) where
    fmap f = EitherT . fmap (fmap f) . runEitherT

instance Applicative f => Applicative (EitherT l f) where
    pure = EitherT . pure . pure
    ef <*> ex = EitherT $ (<*>) <$> runEitherT ef <*> runEitherT ex

instance Monad m => Monad (EitherT l m) where
    return = EitherT . return . return
    x >>= f = EitherT $ do
               a <- runEitherT x
               case a of
                 Left l  -> return (Left l)
                 Right r -> runEitherT (f r)

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

main :: IO ()
main = do
       ergebnis <- runEitherT testEither
       putStrLn $ "Fehlertest == " ++ show ergebnis

testEither :: EitherT String IO Int
testEither = do
         a <- get10
         liftIO . putStrLn $ "10: " ++ show a
         errorTest

get10 :: EitherT String IO Int
get10 = return 10

errorTest :: EitherT String IO Int
errorTest = EitherT . return . Left $ "Fehlertest"
