module Monkey where

import Control.Monad

type Sailor = Int
type Nuts = Integer

nutWithoutMonkey :: Sailor -> Nuts -> Maybe ()
nutWithoutMonkey s a = if a `mod` (fromIntegral s) == 0 then return () else Nothing

nutWithMonkey :: Sailor -> Nuts -> Maybe Nuts
nutWithMonkey s a = if a `mod` fromIntegral s == 1 then return ((a `div` fromIntegral s) * (fromIntegral s-1)) else Nothing

sm :: Sailor -> Nuts -> Maybe Nuts
sm s n = do
         foldl1 (>=>) (replicate s $ nutWithMonkey s) >=> nutWithoutMonkey s $ n
         return n
      

solution :: Sailor -> Maybe Nuts
solution s = msum $ sm s <$> [1..]
