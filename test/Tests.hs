{-# LANGUAGE DoAndIfThenElse #-}

module Main(main) where

import Data.List

import System.Process

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

doctest :: [String] -> IO ()
doctest [] = return ()
doctest (('>':'>':'>':' ':command):rest) = do
    value <- readProcess "./N" ["-e", command] ""
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
