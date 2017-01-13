module Main where

import System.Environment
import System.Exit
import Control.Monad
import System.Console.Haskeline

import qualified Data.Map as M
import Data.List((\\))

import Environment
import Interpreter

main :: IO ()
main = do
    args <- getArgs
    Parameters int pro <- parseParameters args
    val <- case pro of
        Nothing -> return . Just $ M.empty
        Just x -> execProgram x M.empty
    unless int exitSuccess
    case val of
        Nothing -> exitWith (ExitFailure 2)
        Just x -> nInteractWith x

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

type Env = M.Map String FObject

data Parameters = Parameters { interactive :: Bool, code :: Maybe String }

data Switches = Switches {switches :: [String], nonSwitches :: [String]}

parseSwitches :: Bool -> [String] -> Switches
parseSwitches _ [] = Switches [] []
parseSwitches False items = Switches [] items
parseSwitches True ("--":rest) = parseSwitches False rest
parseSwitches True (top:rest)
        = case top of
            '-':'-':tag -> Switches (tag:s) n
            '-':tag -> Switches (tag:s) n
            nonTag -> Switches s (nonTag:n)
    where
    Switches s n = parseSwitches True rest

pFromS :: Switches -> IO Parameters
pFromS (Switches [] []) = return $ Parameters True Nothing
pFromS (Switches ["i"] []) = return $ Parameters True Nothing
pFromS (Switches tags [item])
    | null $ tags \\ ["i", "e"] = Parameters ("i" `elem` tags) . Just <$> if "e" `elem` tags then return item else readFile item 
    | otherwise = usage
pFromS _ = usage

parseParameters :: [String] -> IO Parameters
parseParameters = pFromS . parseSwitches True

usage :: IO a
usage = do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ " [ -i ] [ FILE | -e PROGRAM ]"
    exitWith (ExitFailure 1)
