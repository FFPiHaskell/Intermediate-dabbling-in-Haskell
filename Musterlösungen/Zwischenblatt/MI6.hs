module MI6 where

type Distance  = Int
type ReqLength = Int

data Instruction = F | B deriving (Show, Eq)

generateSteps :: Distance -> ReqLength -> [[Instruction]]
generateSteps d n = recNextSteps n [[]]
  where
    recNextSteps :: Int -> [[Instruction]] -> [[Instruction]]
    recNextSteps 0 is = is
    recNextSteps p is = let nuis = [i ++ [ni] | i <- is, ni <- [F,B]]
                         in recNextSteps (p-1) $ filter (checkSteps d) nuis
 
    checkSteps :: Distance -> [Instruction] -> Bool
    checkSteps d xs = (and . (map (check d 0))) [ dropList q xs | q <- [1..length xs]]
   
    check :: Distance -> Int -> [Instruction] -> Bool
    check n m []                             = (n /= m) && (n /= (-m))
    check n m (i:is) | (n == m) || (n == -m) = False
                     | (i == F)              = check n (m+1) is
                     | otherwise             = check n (m-1) is

    dropList :: Int -> [a] -> [a] 
    dropList k xs = if length xs >=k then let l = drop (k-1) xs in head l : dropList k (tail l) else []

test :: Int -> [[Instruction]]
test n = generateSteps 2 n

main :: IO ()
main = print $ head $ test 11
