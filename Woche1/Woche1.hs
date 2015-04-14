module Woche1 where

--- Aufgabe 1.1

-- point-free
caesar :: Int -> String -> String
caesar n = map (toEnum . (+n) . fromEnum)

--- Aufgabe 1.2

filterFile :: (String -> Bool) -> FilePath -> FilePath -> IO ()
filterFile pred input output = do
  cont <- readFile input
  writeFile output $ (unlines . (filter pred) . lines) cont

-- Hackerstolz: In einer Zeile. Das müsst ihr noch nicht unbedingt 
filterFile' :: (String -> Bool) -> FilePath -> FilePath -> IO ()
filterFile' p iF oF = readFile iF >>= (\ str -> writeFile oF ((unlines . (filter p) . (lines)) str))

--- Aufgabe 1.3

-- (a)

infinitePrimes :: [Integer]
infinitePrimes = sieb [1..] 
  where
    sieb :: [Integer] -> [Integer]
    sieb []     = []
    sieb (i:is) = i : sieb is

-- :-P
finitePrimes :: Int -> [Integer]
finitePrimes n = take n infinitePrimes

-- (b)

primfaktoren :: Integer -> [Integer]
primfaktoren = undefined

goldbach :: Integer -> (Integer, Integer)
goldbach = undefined
