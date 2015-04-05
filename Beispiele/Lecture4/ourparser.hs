{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative

data Parser a = 
     Parser {
            runParser :: String -> (Either String a, String)
     }

instance Functor Parser where
  fmap f p = Parser $ \input ->
               let
                 (res, rem) = runParser p input
               in
                 (fmap f res, rem)

instance Applicative Parser where
  pure a    = Parser $ \input -> (Right a,input)
  pf <*> pa = Parser $ \input ->
                let
                  (rf, rem1) = runParser pf input
                  (ra, rem2) = runParser pa rem1
                in
                  (rf <*> ra, rem2)

instance Monad Parser where
  return a  = pure a
  pa >>= f  = Parser $ \input ->
                let
                  (ra, rem1) = runParser pa input
                in
                  case ra of
                    Right a  -> runParser (f a) rem1
                    Left err -> (Left err, rem1)

instance Alternative Parser where
  empty     = Parser $ \inp -> (Left "",inp)
  pa <|> pb = Parser $ \input -> 
                let
                  (ra, rem1) = runParser pa input
                in
                  case ra of
                    Right _ -> (ra, rem1)
                    _       -> runParser pb input

parse1 :: Parser Int
parse1 = Parser $ \case
                    ('1':xs) -> (Right 1,xs)
                    x        -> (Left "no 1",x)

parse0 :: Parser Int
parse0 = Parser $ \case
                    ('0':xs) -> (Right 0, xs)
                    x        -> (Left "no 0", x)

parseBin :: Parser Int
parseBin = parse0 <|> parse1

testParser :: Parser [Int]
testParser = many parseBin

main :: IO ()
main = do
       print $ runParser testParser "101001"
