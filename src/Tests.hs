module Tests(test) where

import Interpreter
import Environment

import Data.List

import Test.QuickCheck
import Test.QuickCheck.Monadic
    
test :: IO ()
test = do
    quickCheck factorial
    quickCheck nonContents
    return ()

factorial :: Property
factorial = monadicIO $ do
    n <- pick arbitrary
    pre $ n >= 0
    x <- run $ runInterpreter "{n@|!@(n1n-$!$*$)(1)0n=$?$$}$" [Number n]
    assert $ case x of
                (Right [Number f]) -> f == product [1..n]
                _ -> False

nonContents :: Property
nonContents = monadicIO $ do
    res <- run $ runInterpreter code []
    assert $ case res of
        (Right [Str s]) -> (sort . nub $ s ++ code) == ['\x20'..'\x7e']
        _ -> False
    where
    code = "ql$#126#32r$-$s$"
