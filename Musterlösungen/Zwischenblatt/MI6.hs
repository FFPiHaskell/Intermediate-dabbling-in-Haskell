module Main where

-- Sprechende Typen
type Distance  = Int
type ReqLength = Int

-- Forwards or backwards
data Instruction = F | B deriving (Show, Eq)

generateSteps :: Distance -> ReqLength -> [[Instruction]]
generateSteps d n = recNextSteps n [[]]
  where
    -- Erstellt (gegeben eine Liste von möglichen Schrittfolgen) alle neuen
    -- möglichen Schrittfolgen (alle alten + F / alle alten + B) und überprüft auf
    -- Todesfälle. Falls gültige Folgen über sind: rekursiver Aufruf
    recNextSteps :: Int -> [[Instruction]] -> [[Instruction]]
    recNextSteps 0 is = is
    recNextSteps p [] = []
    recNextSteps p is = let new_is = [i ++ [ni] | i <- is, ni <- [F,B]]
                         in recNextSteps (p-1) $ filter (checkSteps d) new_is

    -- Alle Möglichen Ausführungen einer Schrittfolge auf Todesfälle überprüfen
    -- (Also alle Intruktionen, nur jede zweite, jede dritte...)
    -- (div (length xs) 2) weil danach nur einzelne Schritte übrig bleiben, welche immer sicher sind
    checkSteps :: Distance -> [Instruction] -> Bool
    checkSteps d xs = (and . (map (check d 0))) [ dropList q xs | q <- [1..(div (length xs) 2)]]

    -- Eine konkrete Schrittfolge auf Todesfälle überprüfen
    check :: Distance -> Int -> [Instruction] -> Bool
    check n m []                             = (n /= m) && (n /= (-m))
    check n m (i:is) | (n == m) || (n == -m) = False
                     | (i == F)              = check n (m+1) is
                     | otherwise             = check n (m-1) is

    -- Nur jede k-te Instruktion zurück geben
    dropList :: Int -> [a] -> [a]
    dropList k xs = if length xs >= k then let l = drop (k-1) xs in head l : dropList k (tail l) else []

-- Distanz zu Klippe / Schlangen ist zwei Schritte
test :: Int -> [[Instruction]]
test n = generateSteps 2 n

main :: IO ()
main = do let n = 11
          let sols = test n
          if sols == [] then putStrLn "Keine Möglichkeit gefunden!"
                        else putStrLn $ "Eine Möglichkeit ist " ++ (show . head) sols
