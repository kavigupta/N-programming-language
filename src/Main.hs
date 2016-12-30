module Main where

import System.Environment
import System.Exit
import Control.Monad
import System.Console.Haskeline

import qualified Data.Map as M

import Environment
import Interpreter

main :: IO ()
main = do
    args <- getArgs
    params <- parseParameters args
    case params of
        Interactive -> nInteract
        Batch program -> void $ execProgram program M.empty

execProgram :: String -> Env -> IO (Maybe Env)
execProgram prog e = do
    res <- fullInterpreter prog e []
    case res of
        Left err -> print err >> return Nothing
        Right (val, []) -> return . Just $ val
        Right (val, top:_) -> print top >> return (Just val)

nInteract :: IO ()
nInteract = nInteractWith M.empty

nInteractWith :: Env -> IO ()
nInteractWith e = do
    prog <- runInputT defaultSettings $ getInputLine "N> "
    case prog of
        Nothing -> exitSuccess
        Just program -> do
            res <- execProgram program e
            case res of
                Nothing -> nInteractWith e
                Just e' -> nInteractWith e'

type Env = M.Map String Object

data Parameters = Interactive | Batch String

parseParameters :: [String] -> IO Parameters
parseParameters ["-i"] = return Interactive
parseParameters ["-e", program] = return $ Batch program
parseParameters [file] = Batch <$> readFile file
parseParameters _ = do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ " [ -i ] | [ -e PROGRAM ] FILE"
    exitWith (ExitFailure 0)
