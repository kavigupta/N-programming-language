module Object (Object(..), objEqual) where

import Data.List(intercalate)

import AST(AST, printCode)

data Object e a =
      Number Integer
    | Nil
    | Str String
    | Code [AST] e
    | Pair (Object e a) (Object e a)
    | PrimitiveFunction String a

instance (Show e) => Show (Object e a) where
    show (Number x) = show x
    show Nil = "()"
    show (Str s) = show s
    show (Code c e) = code ++ ":" ++ show e
        where
        code = "[" ++ intercalate "," (map printCode c) ++ "]"
    show (Pair car cdr) = "(" ++ withNoParens car cdr ++ ")"
    show (PrimitiveFunction name _) = "#" ++ name

objEqual :: Object e a -> Object e a -> Bool
objEqual (Number x) (Number y)  = x == y
objEqual (Number _) _           = False
objEqual (Str x) (Str y)        = x == y
objEqual (Str _) _              = False
objEqual Nil Nil                = True
objEqual Nil _                  = False
objEqual (Pair a b) (Pair c d)  = objEqual a c && objEqual b d
objEqual (Pair _ _) _           = False
objEqual (Code _ _) (Code _ _)  = False
objEqual (Code _ _) _           = False
objEqual (PrimitiveFunction x _) (PrimitiveFunction y _) = x == y
objEqual (PrimitiveFunction _ _) _ = False

withNoParens :: (Show e) => Object e a -> Object e a -> String
withNoParens car Nil = show car
withNoParens car (Pair cadr cddr) = show car ++ " " ++ withNoParens cadr cddr
withNoParens car cdr = show car ++ " . " ++ show cdr
