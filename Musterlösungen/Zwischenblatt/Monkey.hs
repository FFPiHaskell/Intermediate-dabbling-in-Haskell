module Main where

type Sailors = Int

validNuts :: Sailors -> Int -> Bool
validNuts k n = checkNuts n k k
  where
    checkNuts :: Int -> Int -> Sailors -> Bool
    checkNuts n 0 k = mod n k == 0
    checkNuts n s k = let newPile = div ((k-1) * (n-1)) k
                       in mod n k == 1 && checkNuts newPile (s-1) k 

main :: IO ()
main = print $ filter (validNuts 5) [1..]
