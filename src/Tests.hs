module Tests(test) where

import Interpreter
import Environment

import Data.List

import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Process
    
test :: IO ()
test = do
    quickCheck factorial
    doctests
    return ()

factorial :: Property
factorial = monadicIO $ do
    n <- pick arbitrary
    pre $ n >= 0
    x <- run $ runInterpreter "!$" [Number n]
    assert $ case x of
                (Right [Number f]) -> f == product [1..n]
                _ -> False

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
