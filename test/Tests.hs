{-# LANGUAGE DoAndIfThenElse #-}

module Main(main) where

import Data.List

import System.Process
import System.Timeout

main :: IO ()
main = do
    doctests
    return ()

doctests :: IO ()
doctests = do
    system "make program"
    strDocs <- readFile "tests/doctest.n"
    let docs = lines strDocs
    doctest docs
    putStrLn "Passed Doctests!"

errorOnTimeout :: String -> Int -> IO a -> IO a
errorOnTimeout name time act = do
    perhaps <- timeout time act
    case perhaps of
        Just x -> return x
        Nothing -> error $ "timeout in " ++ name

doctest :: [String] -> IO ()
doctest [] = return ()
doctest (('>':'>':'>':' ':command):rest) = do
    value <- errorOnTimeout command 1000000 $ readProcess "./N" ["-e", command] ""
    let expected = takeWhile (not . isPrefixOf ">>>") rest
    let actual = lines value
    if actual /= expected then
        error $
            "Invalid command " ++ command
                ++ "\nexpected: " ++ show expected
                ++ "\nbut was:  " ++ show actual
    else
        doctest $ dropWhile (not . isPrefixOf ">>>") rest
doctest _ = error "Invalid doctest.n format"
