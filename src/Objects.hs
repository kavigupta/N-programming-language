module Objects(Object(..), objEqual) where

data Object =
      Number Integer
    | Nil
    | Str String
    | Code [AST] Environment
    | Pair Object Object
    | PrimitiveFunction String (InterpAct ())

instance Show Object where
    show (Number x) = show x
    show Nil = "()"
    show (Str s) = show s
    show (Environment.Code c e) = "[" ++ L.intercalate "," (map printCode c) ++ "]" ++ show e
    show (Pair car cdr) = "(" ++ withNoParens car cdr ++ ")"
    show (PrimitiveFunction name _) = "#" ++ name
