module Main where

import Control.Monad
import System.IO
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Data.Functor

data Input = Up
           | Down
           | Quit
           | Invalid
           deriving (Show, Eq)

data Env = Env
           { upKey   :: Char
           , downKey :: Char
           , quitKey :: Char
           }

env :: Env
env = Env 'u' 'd' 'q'

data State = State 
           { counter :: Int
           }

initialState :: State
initialState = State 0

getInput :: RWST Env () State IO Input
getInput = do
            c <- liftIO getChar
            e <- ask     -- get env from ReaderT
            return $ getInputfromEnv c e

getInputfromEnv :: Char -> Env -> Input
getInputfromEnv c e
                | c ==  upKey e   =  Up
                | c ==  downKey e =  Down
                | c ==  quitKey e =  Quit
                | otherwise       =  Invalid

main :: IO ()
main = do
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        score <- fst <$> execRWST mainLoop env initialState
        putStrLn $ "Your Score was: " ++ show (counter score)

mainLoop :: RWST Env () State IO ()
mainLoop = do
        i <- getInput
        case i of
           Up   -> modify (State.((+) 1).counter)
           Down -> modify (State.((+)(-1)).counter)
           _    -> modify id
        s <- get
        liftIO $ putStrLn $ show i ++ ": " ++ show (counter s)
        unless (i == Quit) mainLoop
