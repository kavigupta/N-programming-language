module Tests(test) where

import Interpreter
import Environment

import Data.List

import Test.QuickCheck
    
test :: IO ()
test = do
    quickCheck factorial
    quickCheck nonContents
    return ()

factorial :: Integer -> Property
factorial n = n >= 0 ==> case runInterpreter "{n@|!@(n1n-$!$*$)(1)0n=$?$$}$" [Number n] of
                (Right [Number f]) -> f == product [1..n]
                _ -> False

nonContents :: Bool
nonContents = case runInterpreter code [] of
        (Right [Str s]) -> (sort . nub $ s ++ code) == ['\x20'..'\x7e']
        _ -> False
    where
    code = "ql$#127#32r$-$s$"