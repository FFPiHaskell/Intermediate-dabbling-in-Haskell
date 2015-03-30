{-# LANGUAGE OverloadedStrings #-}
import Data.Time
import Data.Word
import Data.Attoparsec.Text
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B (readFile)
import Control.Applicative

data Datum = Datum
             { tag  :: Day
             , zeit :: TimeOfDay
             } deriving (Show, Eq)


data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)

data Geraet = Mouse 
            | Keyboard 
            | Monitor 
            | Speakers 
            deriving (Show,Eq)

data LogZeile = LogZeile Datum IP Geraet deriving Show

type Log = [LogZeile]


zeitParser :: Parser Datum
zeitParser = do
  y  <- count 4 digit; char '-'
  mm <- count 2 digit; char '-'
  d  <- count 2 digit; char ' '
  h  <- count 2 digit; char ':'
  m  <- count 2 digit; char ':'
  s  <- count 2 digit;
  return $
    Datum { tag  = fromGregorian (read y) (read mm) (read d)
          , zeit = TimeOfDay (read h) (read m) (read s)
          }

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

geraetParser :: Parser Geraet
geraetParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)

zeilenParser :: Parser LogZeile
zeilenParser = do
     datum <- zeitParser
     char ' '
     ip <- parseIP
     char ' '
     geraet <- geraetParser
     return $ LogZeile datum ip geraet

logParser :: Parser Log
logParser = many $ zeilenParser <* endOfLine

main :: IO ()
main = do
     log <- B.readFile "log.txt"
     print $ parseOnly logParser (decodeUtf8 log)
